package dotty.tools
package repl
package linked

/** The compiled-in exclusion list bounding what the synthesized touch root
 *  ([[TouchRoot]]) is allowed to reference.
 *
 *  The reference-graph pre-filter ([[TouchRoot.computeBad]]) already removes every
 *  member that transitively reaches a class/member ABSENT from the Scala.js IR. The
 *  entries here are the residual surface the pre-filter cannot see:
 *
 *   - signature-only references to absent types (Java stream steppers →
 *     `PrimitiveIterator`), which appear in method *signatures* not bodies;
 *   - synthetic dispatch to a *nonexistent method on a present class*
 *     (`Ordering.orElse` forwarders on `Numeric`) — the pre-filter sees absent
 *     *classes*, not absent *methods*;
 *   - Scala.js compiler intrinsics resolved by the emitter, never linked as methods
 *     (`js.await`/`js.import`/`linkTimeIf`);
 *   - JVM-only islands (subprocess, file I/O, weak refs, concurrent maps, blocking);
 *   - emitter short-name collisions (`scala.Function` → `F` clashes with the `Float`
 *     type code) — under per-class modules (SmallestModules) these are, per the
 *     spike's phase-2.5 finding, no longer strictly required, but kept here because
 *     over-excluding only shrinks coverage, never breaks the byte-diff oracle.
 *
 *  Ported verbatim from the JVM spike's `linker-driver/exclusions25.txt` (grouped by
 *  category, one `owner member` pair per line). The fork's runtime classpath
 *  (`scala-library-sjs` + `scalajs-javalib` + `scalajs-library`) is a slightly
 *  different set than the spike's, so the residue may need to grow; adjust [[raw]].
 *
 *  Line grammar (parsed by [[parse]], matched by [[TouchRoot.synthesize]]):
 *   - `owner *`     — exclude every method of the class named exactly `owner`
 *   - `owner **`    — exclude every class whose name starts with `owner`
 *   - `owner name~` — exclude methods of `owner` whose simple name starts with `name`
 *   - `* name`      — exclude the method `name` on ANY class
 *   - `* name~`     — exclude methods whose simple name starts with `name`, any class
 *   - `owner name`  — exclude the specific method `name` on `owner`
 */
object Exclusions:

  /** The exclusion source, grouped/commented like the spike's file. */
  val raw: String =
    """
      |# -- Java Streams/Steppers bridge methods (reference java.util.stream / Spliterator / PrimitiveIterator) --
      |* stepper~
      |* spliterator~
      |* javaIterator~
      |* trySplit~
      |* forEachRemaining~
      |* mkStream~
      |* asJavaSeq~
      |* asJavaPar~
      |* asJavaPrimitive~
      |scala.collection.convert.StreamExtensions **
      |scala.collection.Stepper **
      |scala.collection.IntStepper **
      |scala.collection.LongStepper **
      |scala.collection.DoubleStepper **
      |scala.collection.AnyStepper **
      |scala.jdk.StreamConverters **
      |scala.jdk.Accumulator **
      |scala.jdk.AnyAccumulator **
      |scala.jdk.IntAccumulator **
      |scala.jdk.LongAccumulator **
      |scala.jdk.DoubleAccumulator **
      |scala.jdk.OptionShape **
      |scala.jdk.javaapi.StreamConverters **
      |
      |# -- Ordering.orElse/orElseBy synthetic lambdas (nonexistent trait-forwarder on Numeric) --
      |scala.math.Ordering$$anonfun$orElse$2 *
      |scala.math.Ordering$$anonfun$orElseBy$2 *
      |* scala$math$Ordering$$$anonfun$orElse$1
      |* scala$math$Ordering$$$anonfun$orElseBy$1
      |
      |# -- Scala.js compiler intrinsics (handled by the emitter, not linked as methods) --
      |scala.scalajs.js.special.package **
      |scala.scalajs.runtime.package **
      |
      |# -- JVM-only islands: subprocess, file I/O, weak refs, concurrent maps, executor impls, deprecated manifests --
      |scala.sys.process. **
      |scala.io.BufferedSource **
      |scala.io.Source **
      |scala.ref. **
      |scala.collection.concurrent. **
      |scala.concurrent.impl.ExecutionContextImpl **
      |scala.concurrent.impl.FutureConvertersImpl **
      |scala.reflect.ClassManifestDeprecatedApis **
      |scala.reflect.ManifestFactory **
      |scala.reflect.Manifest **
      |scala.reflect.ClassManifestFactory **
      |# -- JVM-only blocking / threading / reflection residuals --
      |scala.concurrent.impl.CompletionLatch **
      |scala.concurrent.SyncVar **
      |scala.concurrent.Future$never$ **
      |* tryAwait0
      |* scalaProps
      |scala.util.control.Exception **
      |scala.sys.ShutdownHookThread **
      |scala.reflect.Selectable **
      |scala.collection.convert.JavaCollectionWrappers **
      |scala.collection.immutable.RedBlackTree$partitioner$1$ *
      |scala.concurrent.Await **
      |scala.concurrent.Channel **
      |scala.concurrent.impl.Promise$DefaultPromise ready
      |scala.concurrent.impl.Promise$DefaultPromise result
      |* ready
      |* result
      |* blocking0
      |scala.scalajs.js.import **
      |scala.scalajs.js.new **
      |scala.scalajs.js.package await
      |scala.scalajs.js.package.package await
      |scala.concurrent.Await$ ready
      |scala.concurrent.Await$ result
      |# Scala.js compiler intrinsics (resolved by the emitter, not linked as methods)
      |scala.scalajs.js.package native
      |scala.scalajs.js.package async
      |scala.scalajs.js.package typeOf
      |scala.scalajs.js.package import
      |* linkTimeIf
      |scala.scalajs.runtime.DynamicImportThunk **
      |* dynamicImport$
      |java.lang.Float$BigIntImpl$JSBigInt multiplyBy10Pow
      |scala.collection.MapFactory$$anon$2 newBuilder
      |scala.concurrent.SyncChannel read
      |scala.reflect.ClassTag$GenericClassTag $less$colon$less
      |scala.scalajs.js.Any$BuildFromArray$1 apply
      |scala.scalajs.LinkingInfo linkTimeIf
      |scala.scalajs.runtime.DynamicImportThunk dynamicImport$
      |java.lang.Float$BigIntImpl$JSBigInt$ multiplyBy10Pow
      |scala.collection.BuildFrom apply
      |scala.concurrent.SyncChannel write
      |scala.reflect.ClassTypeManifest $less$colon$less
      |scala.collection.StringOps$ toFloat$extension
      |scala.concurrent.Future$ traverse
      |scala.reflect.ClassTag$GenericClassTag $greater$colon$greater
      |java.lang.Float$ parseFloat
      |scala.concurrent.Future$ sequence
      |scala.reflect.ClassTypeManifest $greater$colon$greater
      |scala.concurrent.Future sequence
      |scala.reflect.AnyValManifest $greater$colon$greater
      |scala.util.FromDigits floatFromDigits
      |scala.concurrent.Future$ reduce
      |scala.util.FromDigits$ floatFromDigits
      |scala.concurrent.Future$ fold
      |scala.io.StdIn readFloat
      |scala.runtime.Tuple3Zipped$ filter$extension
      |scala.util.CommandLineParser$FromString$given_FromString_Float$ fromString
      |scala.collection.StringOps$ toFloatOption$extension
      |scala.concurrent.Future traverse
      |scala.collection.StringParsers$ parseFloat
      |scala.concurrent.Future fold
      |# deprecated Manifest reflective subtype path (reaches absent Class.getInterfaces
      |# through equals->'<:<'->subtype->loop$1; a normal link never instantiates these)
      |* $less$colon$less
      |* $greater$colon$greater
      |* subtype
      |scala.reflect.ManifestFactory$ClassTypeManifest **
      |scala.reflect.ManifestFactory$SingletonTypeManifest **
      |scala.reflect.ManifestFactory$AbstractTypeManifest **
      |scala.reflect.ManifestFactory$WildcardManifest **
      |scala.reflect.ManifestFactory$IntersectionTypeManifest **
      |* classType
      |* singleType
      |* abstractType
      |* intersectionType
      |* wildcardType
      |* subType
      |scala.reflect.package **
      |scala.reflect.NoManifest **
      |scala.reflect.AnyValManifest **
      |scala.reflect.ClassTag$GenericClassTag **
      |scala.collection.mutable.WeakHashMap **
      |scala.collection.mutable.WeakHashMap$ **
      |scala.sys. **
      |scala.sys.Prop **
      |scala.sys.SystemProperties **
      |# emitter short-name collides with primitive type-code F / T
      |scala.Function *
      |scala.Tuple *
    """.stripMargin

  /** Parse [[raw]] into `(owner, member)` pairs, dropping comments and blanks. */
  def parse(text: String): Set[(String, String)] =
    text.linesIterator.map(_.trim)
      .filter(l => l.nonEmpty && !l.startsWith("#"))
      .flatMap { l =>
        val parts = l.split("\\s+")
        if parts.length >= 2 then Some((parts(0), parts(1))) else None
      }
      .toSet

  /** The default compiled-in exclusion set. */
  val default: Set[(String, String)] = parse(raw)
