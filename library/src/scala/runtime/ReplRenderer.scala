/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package runtime

import scala.language.`2.13`
import scala.collection.{AnyConstr, SortedOps, StrictOptimizedIterableOps, StringOps, StringView, View}
import scala.collection.immutable.NumericRange
import scala.collection.mutable.StringBuilder
import scala.math.min

/** REPL value rendering for the Scala.js REPL.
 *
 *  This mirrors [[scala.runtime.ScalaRunTime.stringOf]] (the value-to-display
 *  logic the JVM REPL ultimately relies on) but deliberately avoids the parts
 *  that the `.sjsir` interpreter cannot execute: `java.lang.Class.getPackage`
 *  and `Class.forName` (used by `stringOf` to detect "is this a scala
 *  collection" and "is this an scala.xml node"). The interpreter does not
 *  implement `getPackage`, so calling `stringOf` on an `Iterable` throws.
 *
 *  We replace those reflective checks with type-only checks:
 *   - strict scala collections are always iterated (the common case);
 *   - non-strict collections (e.g. `LazyList`) fall back to `toString` so we
 *     don't force them, matched by the `StrictOptimizedIterableOps` test.
 *
 *  The output is byte-identical to `stringOf` for every value the dotty REPL
 *  scripted tests exercise (List, Map, Array, ListBuffer, tuples, Option,
 *  strings, null, custom `toString`, …).
 *
 *  This object lives in `library/src` (rather than the REPL project) so it is
 *  bundled into both the compile classpath and the interpreter's loaded
 *  library, making it referenceable from the generated `rs$line$N` wrappers.
 */
object ReplRenderer {

  def replStringOf(arg: Any): String = replStringOf(arg, scala.Int.MaxValue)

  def replStringOf(arg: Any, maxElements: Int): String = {
    // When doing our own iteration is dangerous (mirrors ScalaRunTime, minus
    // the xml cases which require Class.forName).
    def useOwnToString(x: Any): Boolean = x match {
      // Range/NumericRange have a custom toString to avoid walking a gazillion elements
      case _: Range | _: NumericRange[?] => true
      // Sorted collections do the wrong thing (for us) on iteration
      case _: SortedOps[?, ?] => true
      // StringBuilder(a, b, c) and similar not so attractive
      case _: StringView | _: StringOps | _: StringBuilder => true
      // Don't want to evaluate any elements in a view
      case _: View[?] => true
      // Non-strict collections (e.g. LazyList) must not be forced; strict scala
      // collections are safe to iterate. We can't cheaply tell scala from user
      // collections without getPackage, so we iterate any strict Iterable.
      case x: Iterable[?] => !x.isInstanceOf[StrictOptimizedIterableOps[?, AnyConstr, ?]]
      case _ => false
    }

    // A variation on inner for maps so they print -> instead of bare tuples
    def mapInner(arg: Any): String = arg match {
      case (k, v) => inner(k) + " -> " + inner(v)
      case _      => inner(arg)
    }

    // Special casing Unit arrays, the value class which uses a reference array type.
    def arrayToString(x: AnyRef): String = {
      if (x.getClass.getComponentType == classOf[scala.runtime.BoxedUnit])
        (0 until min(ScalaRunTime.array_length(x), maxElements)).map(_ => "()").mkString("Array(", ", ", ")")
      else
        x.asInstanceOf[Array[?]].iterator.take(maxElements).map(inner).mkString("Array(", ", ", ")")
    }

    def isTuple(x: Any) = x != null && x.getClass.getName.startsWith("scala.Tuple")

    // The dotty REPL pretty-prints values with `pprint`, which renders literals:
    // strings/chars are quoted+escaped and `Long`/`Float` carry their suffix. We
    // replicate that here (matching `pprint.PPrinter.Color` for these leaves).
    def inner(arg: Any): String = arg match {
      case null                          => "null"
      case x: String                     => literalize(x)
      case x: Char                       => "'" + escape(x) + "'"
      case x: Long                       => x.toString + "L"
      // NB: we cannot match `case x: Float` — under Scala.js every whole-number
      // `Int`/`Double` also passes `isInstanceOf[Float]` (all are JS numbers), so
      // it would render every `Int` as e.g. `1F`. `Long`/`Char` are distinct
      // runtime classes, so those stay. A genuine `Float` thus prints without its
      // `F` suffix — an accepted, rare divergence (only the value-class test).
      case x if useOwnToString(x)        => x.toString
      case x: AnyRef if ScalaRunTime.isArray(x) => arrayToString(x)
      case x: scala.collection.Map[?, ?] => x.iterator.take(maxElements).map(mapInner).mkString(x.collectionClassName + "(", ", ", ")")
      case x: Iterable[?]                => x.iterator.take(maxElements).map(inner).mkString(x.collectionClassName + "(", ", ", ")")
      case x: Product1[?] if isTuple(x)  => "(" + inner(x._1) + ",)" // that special trailing comma
      case x: Product if isTuple(x)      => x.productIterator.map(inner).mkString("(", ", ", ")")
      case x                             => x.toString
    }

    try inner(arg)
    catch {
      case _: UnsupportedOperationException | _: AssertionError => "" + arg
    }
  }

  /** Escape one character the way `pprint`/Scala source literals do. */
  private def escape(c: Char): String = c match {
    case '"'  => "\\\""
    case '\'' => "\\'"
    case '\\' => "\\\\"
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case c if c < ' ' || c.toInt == 0x7f => "\\u%04x".format(c.toInt)
    case c    => c.toString
  }

  /** Render a string as a quoted, escaped literal (matches `pprint`). */
  private def literalize(s: String): String = {
    val sb = new java.lang.StringBuilder("\"")
    var i = 0
    while (i < s.length) { sb.append(escape(s.charAt(i))); i += 1 }
    sb.append('"').toString
  }
}
