package dotty.tools
package repl

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.*

import dotc.ClasspathBlob
import dotc.core.Contexts.*
import dotc.core.Phases.Phase
import dotc.config.{Platform, SJSPlatform}
import dotc.classpath.{AggregateClassPath, VirtualDirectoryClassPath}
import dotc.util.SourceFile
import dotty.tools.io.{AbstractFile, VirtualDirectory}

/** Tests for extra-library preloading (`--classpath` / `DOTTY_EXTRA_LIBS_BIN` /
 *  [[ExtraLib]]), run as part of [[EvalScriptedTests]].
 *
 *  Three layers:
 *   - archive-format unit tests: a test-local writer of the packed `.bin`
 *     format (mirroring `writeBinArchive` in project/Build.scala) round-tripped
 *     through [[ClasspathBlob]], plus CLI/env parsing in [[ReplBootstrap]];
 *   - fixture compilation: a small library is compiled in-process with
 *     `-scalajs` (the same compiler that serves the REPL) into `.tasty` +
 *     `.sjsir`, then packed — no pre-built binary fixtures to go stale;
 *   - end-to-end sessions: [[ReplSession]]s created with the fixture libs
 *     preloaded, exercising typechecking, execution, cross-lib dependencies,
 *     shadowing order, `:reset` persistence, the env-var bootstrap path, and
 *     native companion JS modules reached through `@js.native` facades (see
 *     [[JSModuleRegistry]]).
 */
object ExtraLibsTests:

  def run(): Future[(Int, Int)] =
    (ReplBootstrap.env("DOTTY_CLASSPATH_BIN"), ReplBootstrap.env("DOTTY_LINKER_LIBS_BIN")) match
      case (Some(cp), Some(lib)) =>
        val cpDir = ClasspathBlob.load(ReplBootstrap.readArrayBuffer(cp))
        val libBuf = ReplBootstrap.readArrayBuffer(lib)
        runChecks(unitChecks).flatMap { case (p0, f0) =>
          compileFixtures(cpDir) match
            case Left(err) =>
              println(s"FAIL extra-libs/fixture-compile")
              println(err.linesIterator.map("    " + _).mkString("\n"))
              Future.successful((p0, f0 + 1))
            case Right(fx) =>
              println(s"PASS extra-libs/fixture-compile")
              for
                (p1, f1) <- runChecks(fixtureChecks(cpDir, fx))
                (p2, f2) <- sessionWithLibsChecks(cpDir, libBuf, fx)
                (p3, f3) <- controlSessionChecks(cpDir, libBuf)
                (p4, f4) <- shadowOrderChecks(cpDir, libBuf, fx)
                (p5, f5) <- envVarChecks(cpDir, libBuf, fx)
                (p6, f6) <- nativeModuleChecks(cpDir, libBuf, fx)
              yield (p0 + 1 + p1 + p2 + p3 + p4 + p5 + p6, f0 + f1 + f2 + f3 + f4 + f5 + f6)
        }
      case _ =>
        println("FAIL extra-libs (set DOTTY_CLASSPATH_BIN and DOTTY_LINKER_LIBS_BIN)")
        Future.successful((0, 1))

  // --- archive format + parsing unit tests -----------------------------------

  private def unitChecks: List[Check] = List(
    Check("archive-round-trip", () => Future.successful {
      val bytes = Array.tabulate[Byte](256)(i => i.toByte) // full byte range
      val entries = List(
        ("a/b/C.tasty", bytes),
        ("empty.class", Array.empty[Byte]),
        ("a/D.sjsir", Array[Byte](1, -2, 3)),
      )
      val back = ClasspathBlob.loadEntries(mkArchive(entries))
      check(back.map(_._1) == entries.map(_._1), s"paths/order mismatch: ${back.map(_._1)}")
        && check(back.zip(entries).forall((b, e) => b._2.sameElements(e._2)), "content mismatch")
    }),
    Check("archive-to-virtual-dir", () => Future.successful {
      val entries = List(("p/q/R.tasty", Array[Byte](42)), ("S.class", Array[Byte](7, 8)))
      val dir = ClasspathBlob.load(mkArchive(entries))
      check(ClasspathBlob.lookupPath(dir, "p/q/R.tasty").exists(_.toByteArray.sameElements(Array[Byte](42))), "nested lookup failed")
        && check(ClasspathBlob.lookupPath(dir, "S.class").isDefined, "top-level lookup failed")
        && check(ClasspathBlob.lookupPath(dir, "p/missing.tasty").isEmpty, "missing file should not resolve")
        && check(ClasspathBlob.lookupPath(dir, "nope/x.tasty").isEmpty, "missing dir should not resolve")
    }),
    Check("entries-of-inverse", () => Future.successful {
      val entries = List(("z/Z.tasty", Array[Byte](1)), ("a/A.tasty", Array[Byte](2)), ("M.sjsir", Array[Byte](3)))
      val back = ClasspathBlob.entriesOf(ClasspathBlob.dirFromEntries("(t)", entries))
      check(back.map(_._1) == List("M.sjsir", "a/A.tasty", "z/Z.tasty"), s"expected sorted paths, got ${back.map(_._1)}")
        && check(back.forall((p, b) => entries.find(_._1 == p).exists(_._2.sameElements(b))), "content mismatch")
    }),
    Check("extra-lib-split", () => Future.successful {
      val lib = ExtraLib.fromArchive("t.bin", mkArchive(List(
        ("p/A.tasty", Array[Byte](1)),
        ("p/A.class", Array[Byte](2)),
        ("p/A.sjsir", Array[Byte](3)),
        ("p/A$.sjsir", Array[Byte](4)),
      )))
      check(lib.sjsir.keySet == Set("p/A.sjsir", "p/A$.sjsir"), s"sjsir split wrong: ${lib.sjsir.keySet}")
        && check(ClasspathBlob.filesUnder(lib.cpDir).map(_._1) == List("p/A.class", "p/A.tasty"), "classpath split wrong")
    }),
    Check("extra-lib-js-module-split", () => Future.successful {
      val lib = ExtraLib.fromArchive("t.bin", mkArchive(List(
        ("p/A.tasty", Array[Byte](1)),
        ("p/A.sjsir", Array[Byte](2)),
        ("js-modules/testNative.js", utf8("exports.marker = \"m\";\n")),
        ("js-modules/notes.txt", Array[Byte](3)), // only `.js` is a module
      )))
      check(lib.jsModules.keySet == Set("testNative"), s"jsModules split wrong: ${lib.jsModules.keySet}")
        && check(lib.jsModules("testNative") == "exports.marker = \"m\";\n", s"module text wrong: ${lib.jsModules("testNative")}")
        && check(lib.sjsir.keySet == Set("p/A.sjsir"), s"sjsir split wrong: ${lib.sjsir.keySet}")
        && check(ClasspathBlob.filesUnder(lib.cpDir).map(_._1) == List("js-modules/notes.txt", "p/A.tasty"),
          s"classpath split wrong: ${ClasspathBlob.filesUnder(lib.cpDir).map(_._1)}")
    }),
    Check("js-module-name-validation", () => Future.successful {
      val valid = List("testNative", "aukGrepEngine", "_$x9", "$", "class")
      val invalid = List("", "auk-grep-engine", "@scope/pkg", "9lives", "a b", "a.b")
      check(valid.forall(JSModuleRegistry.isValidModuleName), s"rejected a valid name: $valid")
        && check(!invalid.exists(JSModuleRegistry.isValidModuleName), s"accepted an invalid name: $invalid")
        && check(
          try { JSModuleRegistry.register("auk-grep-engine", "exports.x = 1;"); false }
          catch case e: IllegalArgumentException => e.getMessage.contains("auk-grep-engine"),
          "register should reject an invalid name, naming it")
    }),
    Check("empty-archive", () => Future.successful {
      val lib = ExtraLib.fromArchive("e.bin", mkArchive(Nil))
      check(ClasspathBlob.loadEntries(mkArchive(Nil)).isEmpty, "expected no entries")
        && check(lib.sjsir.isEmpty && lib.jsModules.isEmpty && ClasspathBlob.filesUnder(lib.cpDir).isEmpty, "expected empty lib")
    }),
    Check("malformed-archive-throws", () => Future.successful {
      val buf = new ArrayBuffer(8)
      new DataView(buf).setUint32(0, 99999) // index length far beyond the buffer
      try { ClasspathBlob.loadEntries(buf); check(false, "expected an exception") }
      catch case _: Throwable => true
    }),
    Check("classpath-arg-parsing", () => Future.successful {
      val r1 = ReplBootstrap.extractClasspathArgs(List("--classpath", "a.bin:b.bin", "--script", "f"))
      val r2 = ReplBootstrap.extractClasspathArgs(List("--classpath=c.bin", "x"))
      val r3 = ReplBootstrap.extractClasspathArgs(List("--classpath", "a.bin", "--classpath", "b.bin"))
      val r4 = ReplBootstrap.extractClasspathArgs(List("--classpath"))
      val r5 = ReplBootstrap.extractClasspathArgs(List("plain", "args"))
      check(r1 == Right((List("a.bin", "b.bin"), List("--script", "f"))), s"r1: $r1")
        && check(r2 == Right((List("c.bin"), List("x"))), s"r2: $r2")
        && check(r3 == Right((List("a.bin", "b.bin"), Nil)), s"r3: $r3")
        && check(r4.isLeft, s"r4 should be an error: $r4")
        && check(r5 == Right((Nil, List("plain", "args"))), s"r5: $r5")
    }),
    Check("path-list-splitting", () => Future.successful {
      check(ReplBootstrap.splitPathList("a.bin:b.bin") == List("a.bin", "b.bin"), "basic split")
        && check(ReplBootstrap.splitPathList("a.bin::b.bin:") == List("a.bin", "b.bin"), "empty segments dropped")
        && check(ReplBootstrap.splitPathList(" a.bin : b.bin ") == List("a.bin", "b.bin"), "trimmed")
        && check(ReplBootstrap.splitPathList("") == Nil, "empty list")
    }),
  )

  // --- fixture libraries ------------------------------------------------------

  private final case class Fixtures(
    libA: ExtraLib, libABuf: ArrayBuffer,
    libB: ExtraLib, libBBuf: ArrayBuffer,
    dup1: ExtraLib, dup2: ExtraLib,
    nativeLib: ExtraLib, dupNativeA: ExtraLib, dupNativeB: ExtraLib,
  )

  private val libASource =
    """package fixturelib
      |
      |class Greeter(val name: String):
      |  def greet: String = s"Hello, $name!"
      |
      |object Counter:
      |  private var n = 0
      |  def next(): Int = { n += 1; n }
      |
      |case class Point(x: Int, y: Int):
      |  def +(other: Point): Point = Point(x + other.x, y + other.y)
      |
      |enum Color:
      |  case Red, Green, Blue
      |
      |object Util:
      |  inline def twice(inline x: Int): Int = x + x
      |  def colors: List[Color] = List(Color.Red, Color.Green, Color.Blue)
      |
      |trait Shape:
      |  def area: Double
      |
      |class Circle(val radius: Double) extends Shape:
      |  def area: Double = 3.0 * radius * radius
      |""".stripMargin

  private val libBSource =
    """package fixturelibb
      |
      |import fixturelib.*
      |
      |object Greetings:
      |  def greetAll(names: List[String]): List[String] =
      |    names.map(n => Greeter(n).greet)
      |""".stripMargin

  private def dupSource(which: String) =
    s"""package duplib
       |
       |object Which:
       |  def which: String = "$which"
       |""".stripMargin

  /** Typed facades over native companion JS modules. `TestNative`'s module is
   *  shipped in the same archive; `MissingNative`'s is deliberately never
   *  shipped; `DupNative`'s is shipped twice, by two later archives. */
  private val nativeFacadeSource =
    """package nativelib
      |
      |import scala.scalajs.js
      |import scala.scalajs.js.annotation.JSImport
      |
      |@js.native
      |@JSImport("testNative", JSImport.Namespace, globalFallback = "__replJSModules.testNative")
      |object TestNative extends js.Object:
      |  val marker: String = js.native
      |  def greet(name: String): String = js.native
      |  def baseName(p: String): String = js.native
      |  def totalLoads(): Int = js.native
      |
      |@js.native
      |@JSImport("missingNative", JSImport.Namespace, globalFallback = "__replJSModules.missingNative")
      |object MissingNative extends js.Object:
      |  def anything(): String = js.native
      |
      |@js.native
      |@JSImport("dupNative", JSImport.Namespace, globalFallback = "__replJSModules.dupNative")
      |object DupNative extends js.Object:
      |  val which: String = js.native
      |""".stripMargin

  /** A CommonJS module: exercises `require`, and counts its own executions in a
   *  global so a second load would be visible through the *first* exports. */
  private val testNativeModule =
    """var path = require("node:path");
      |globalThis.__testNativeLoads = (globalThis.__testNativeLoads || 0) + 1;
      |exports.marker = "native-marker";
      |exports.greet = function (name) { return "Hello from JS, " + name + "!"; };
      |exports.baseName = function (p) { return path.basename(p); };
      |exports.totalLoads = function () { return globalThis.__testNativeLoads | 0; };
      |""".stripMargin

  private def dupNativeModule(which: String) =
    s"""exports.which = "$which";
       |""".stripMargin

  private def compileFixtures(cpDir: VirtualDirectory): Either[String, Fixtures] =
    for
      aOut <- FixtureDriver(List(cpDir)).compileVirtual(List(("FixtureLib.scala", libASource)))
      bOut <- FixtureDriver(List(cpDir, aOut)).compileVirtual(List(("FixtureLibB.scala", libBSource)))
      d1   <- FixtureDriver(List(cpDir)).compileVirtual(List(("Dup1.scala", dupSource("first"))))
      d2   <- FixtureDriver(List(cpDir)).compileVirtual(List(("Dup2.scala", dupSource("second"))))
      nOut <- FixtureDriver(List(cpDir)).compileVirtual(List(("NativeFacades.scala", nativeFacadeSource)))
    yield
      val aBuf = mkArchive(ClasspathBlob.entriesOf(aOut))
      val bBuf = mkArchive(ClasspathBlob.entriesOf(bOut))
      // The facade archive carries its companion `.js` alongside the compiled
      // halves — the shape `packLibraryBin` produces.
      val nBuf = mkArchive(ClasspathBlob.entriesOf(nOut)
        :+ (ExtraLib.JSModulePrefix + "testNative.js", utf8(testNativeModule)))
      def dupNativeArchive(which: String) =
        mkArchive(List((ExtraLib.JSModulePrefix + "dupNative.js", utf8(dupNativeModule(which)))))
      Fixtures(
        ExtraLib.fromArchive("liba.bin", aBuf), aBuf,
        ExtraLib.fromArchive("libb.bin", bBuf), bBuf,
        ExtraLib.fromArchive("dup1.bin", mkArchive(ClasspathBlob.entriesOf(d1))),
        ExtraLib.fromArchive("dup2.bin", mkArchive(ClasspathBlob.entriesOf(d2))),
        ExtraLib.fromArchive("native.bin", nBuf),
        ExtraLib.fromArchive("dupa.bin", dupNativeArchive("first")),
        ExtraLib.fromArchive("dupb.bin", dupNativeArchive("second")),
      )

  /** Sanity checks on the packed fixtures + the shadow-detection helper. */
  private def fixtureChecks(cpDir: VirtualDirectory, fx: Fixtures): List[Check] = List(
    Check("fixture-has-tasty-and-sjsir", () => Future.successful {
      val paths = ClasspathBlob.filesUnder(fx.libA.cpDir).map(_._1)
      check(paths.contains("fixturelib/Greeter.tasty"), s"missing Greeter.tasty in $paths")
        && check(fx.libA.sjsir.keySet.exists(_.endsWith("Greeter.sjsir")), s"missing Greeter.sjsir in ${fx.libA.sjsir.keySet}")
        && check(fx.libA.sjsir.keySet.exists(_.endsWith("Counter$.sjsir")), "missing module class sjsir")
    }),
    Check("shadowed-paths-between-libs", () => Future.successful {
      val shadowed = ExtraLib.shadowedPaths(cpDir, List(fx.dup1, fx.dup2))
      check(shadowed.exists((lib, p) => lib == "dup2.bin" && p == "duplib/Which.tasty"),
        s"expected dup2.bin duplib/Which.tasty flagged, got $shadowed")
        && check(!shadowed.exists(_._1 == "dup1.bin"), s"first lib must not be flagged: $shadowed")
    }),
    Check("shadowed-paths-vs-stdlib", () => Future.successful {
      val rogue = ExtraLib.fromArchive("rogue.bin", mkArchive(List(("scala/Option.tasty", Array[Byte](0)))))
      val shadowed = ExtraLib.shadowedPaths(cpDir, List(rogue))
      check(shadowed == List(("rogue.bin", "scala/Option.tasty")), s"expected stdlib shadow flagged, got $shadowed")
    }),
    Check("no-false-shadow-reports", () => Future.successful {
      check(ExtraLib.shadowedPaths(cpDir, List(fx.libA, fx.libB)).isEmpty, "fixture libs should not be flagged")
    }),
  )

  // --- end-to-end sessions ----------------------------------------------------

  /** The main session: libA + libB preloaded; exercises compile + run + render. */
  private def sessionWithLibsChecks(cpDir: VirtualDirectory, libBuf: ArrayBuffer, fx: Fixtures): Future[(Int, Int)] =
    ReplSession.create(cpDir, libBuf, List(fx.libA, fx.libB)).flatMap { s =>
      runChecks(List(
        Check("s1-import-lib", () => expectOutput(s, "import fixturelib.*", "")),
        Check("s1-instantiate-class", () => s.eval("""val g = Greeter("World")""").map { r =>
          check(r.ok && r.output.trim.startsWith("val g: Greeter = ") && r.output.contains("Greeter@"),
            s"got ok=${r.ok} output=${r.output.trim}")
        }),
        Check("s1-call-method", () => expectOutput(s, "g.greet", """val res0: String = "Hello, World!"""")),
        Check("s1-lib-state", () => expectOutput(s, "Counter.next()", "val res1: Int = 1")),
        Check("s1-lib-state-persists", () => expectOutput(s, "Counter.next()", "val res2: Int = 2")),
        Check("s1-case-class", () => expectOutput(s, "val p = Point(1, 2) + Point(3, 4)", "val p: Point = Point(4,6)")),
        Check("s1-pattern-match", () => expectOutput(s, "p match { case Point(x, y) => x + y }", "val res3: Int = 10")),
        Check("s1-inline-def", () => expectOutput(s, "Util.twice(21)", "val res4: Int = 42")),
        Check("s1-enum", () => expectOutput(s, "Util.colors", "val res5: List[Color] = List(Red, Green, Blue)")),
        Check("s1-extend-lib-trait", () => expectOutput(s,
          "class Square(side: Double) extends Shape { def area = side * side }", "// defined class Square")),
        Check("s1-use-subclass", () => expectOutput(s, "Square(1.5).area", "val res6: Double = 2.25")),
        Check("s1-mixed-lib-and-user", () => s.eval("val shapes: List[Shape] = List(Circle(2.0), Square(3.0))").map { r =>
          check(r.ok && r.output.trim.startsWith("val shapes: List[Shape] = List("), s"got ok=${r.ok} output=${r.output.trim}")
        }),
        Check("s1-cross-lib-call", () => expectOutput(s,
          """fixturelibb.Greetings.greetAll(List("a", "b"))""",
          """val res7: List[String] = List("Hello, a!", "Hello, b!")""")),
        Check("s1-missing-member-errors", () => s.eval("Util.nope").map { r =>
          check(!r.ok && r.error.exists(_.contains("nope")), s"got ok=${r.ok} error=${r.error}")
        }),
        Check("s1-json-protocol", () => JsonProtocol.respond(s, JsonProtocol.evalRequest("Util.twice(2)")).map { (json, _) =>
          check(json.contains(""""ok":true""") && json.contains("res9: Int = 4"), s"got $json")
        }),
        Check("s1-reset-keeps-lib", () => s.reset().flatMap(_ =>
          expectOutput(s, "fixturelib.Counter.next()", "val res0: Int = 1"))),
        Check("s1-reset-fresh-import", () => s.eval("import fixturelib.*").flatMap(_ =>
          expectOutput(s, "Util.twice(3)", "val res1: Int = 6"))),
      ))
    }

  /** Control: without extras the fixture package must not exist. */
  private def controlSessionChecks(cpDir: VirtualDirectory, libBuf: ArrayBuffer): Future[(Int, Int)] =
    ReplSession.create(cpDir, libBuf, Nil).flatMap { s =>
      runChecks(List(
        Check("control-lib-absent", () => s.eval("import fixturelib.*").map { r =>
          check(!r.ok && r.error.exists(_.contains("fixturelib")), s"got ok=${r.ok} error=${r.error}")
        }),
        Check("control-stdlib-works", () => expectOutput(s, "List(1, 2, 3).sum", "val res0: Int = 6")),
      ))
    }

  /** Two libs defining the same class: the first on the classpath must win,
   *  consistently at compile time and in the interpreter. */
  private def shadowOrderChecks(cpDir: VirtualDirectory, libBuf: ArrayBuffer, fx: Fixtures): Future[(Int, Int)] =
    ReplSession.create(cpDir, libBuf, List(fx.dup1, fx.dup2)).flatMap { s12 =>
      runChecks(List(
        Check("shadow-first-wins", () => expectOutput(s12, "duplib.Which.which", """val res0: String = "first"""")),
      ))
    }.flatMap { case (p1, f1) =>
      ReplSession.create(cpDir, libBuf, List(fx.dup2, fx.dup1)).flatMap { s21 =>
        runChecks(List(
          Check("shadow-order-matters", () => expectOutput(s21, "duplib.Which.which", """val res0: String = "second"""")),
        ))
      }.map { case (p2, f2) => (p1 + p2, f1 + f2) }
    }

  /** The `DOTTY_EXTRA_LIBS_BIN` bootstrap path, with real files on disk. */
  private def envVarChecks(cpDir: VirtualDirectory, libBuf: ArrayBuffer, fx: Fixtures): Future[(Int, Int)] =
    val pathA = writeTempFile("liba.bin", fx.libABuf)
    val pathB = writeTempFile("libb.bin", fx.libBBuf)

    def withEnv[A](value: String)(body: => Future[A]): Future[A] =
      val envObj = js.Dynamic.global.process.env
      val old = ReplBootstrap.env("DOTTY_EXTRA_LIBS_BIN")
      envObj.updateDynamic("DOTTY_EXTRA_LIBS_BIN")(value)
      def restore(): Unit = old match
        case Some(v) => envObj.updateDynamic("DOTTY_EXTRA_LIBS_BIN")(v)
        case None    => js.special.delete(envObj, "DOTTY_EXTRA_LIBS_BIN")
      // The env var is read synchronously inside createSessionFromEnv, so it is
      // safe to restore as soon as `body`'s future is constructed.
      try { val f = body; restore(); f }
      catch { case e: Throwable => restore(); throw e }

    runChecks(List(
      Check("env-var-two-libs", () => withEnv(s"$pathA:$pathB") {
        ReplBootstrap.createSessionFromEnv() match
          case None => Future.successful(check(false, "createSessionFromEnv returned None"))
          case Some(sessionF) => sessionF.flatMap(s =>
            expectOutput(s, """fixturelibb.Greetings.greetAll(List("env"))""",
              """val res0: List[String] = List("Hello, env!")"""))
      }),
      Check("env-var-missing-file", () => withEnv("/nonexistent/zzz.bin") {
        ReplBootstrap.createSessionFromEnv() match
          case None => Future.successful(check(false, "createSessionFromEnv returned None"))
          case Some(sessionF) => sessionF
            .map(_ => check(false, "expected session creation to fail"))
            .recover { case e => check(e.getMessage.contains("/nonexistent/zzz.bin"), s"message: ${e.getMessage}") }
      }),
      Check("classpath-args-feed-session", () =>
        // Same load path the CLI/JSON mains use for `--classpath` values.
        ReplBootstrap.createSessionFromEnv(extraLibArgs = List(s"$pathA:$pathB")) match
          case None => Future.successful(check(false, "createSessionFromEnv returned None"))
          case Some(sessionF) => sessionF.flatMap(s =>
            expectOutput(s, "fixturelib.Util.twice(8)", "val res0: Int = 16"))
      ),
    ))

  /** Native companion JS modules: an archive-shipped CommonJS module reached
   *  through a typed `@js.native @JSImport(…, globalFallback = …)` facade, plus
   *  the failure modes — an unshipped module, and a module name claimed twice. */
  private def nativeModuleChecks(cpDir: VirtualDirectory, libBuf: ArrayBuffer, fx: Fixtures): Future[(Int, Int)] =
    // Registration warnings are printed synchronously by `create`, before the
    // returned future completes, so the capture can be lifted straight away.
    val (sessionF, warnings) =
      captureStderr(ReplSession.create(cpDir, libBuf, List(fx.nativeLib, fx.dupNativeA, fx.dupNativeB)))
    sessionF.flatMap { s =>
      runChecks(List(
        Check("native-modules-registered", () => Future.successful {
          check(JSModuleRegistry.registered == List("dupNative", "testNative"),
            s"registered: ${JSModuleRegistry.registered}")
        }),
        Check("native-duplicate-warning", () => Future.successful {
          check(warnings.contains("warning: dupb.bin: JS module 'dupNative' is already registered and ignored"),
            s"stderr was: $warnings")
        }),
        Check("native-call-method", () => expectOutput(s,
          """nativelib.TestNative.greet("REPL")""", """val res0: String = "Hello from JS, REPL!"""")),
        Check("native-read-property", () => expectOutput(s,
          "nativelib.TestNative.marker", """val res1: String = "native-marker"""")),
        Check("native-module-used-require", () => expectOutput(s,
          """nativelib.TestNative.baseName("/tmp/a/b/engine.js")""", """val res2: String = "engine.js"""")),
        Check("native-module-ran-once", () => expectOutput(s,
          "nativelib.TestNative.totalLoads()", "val res3: Int = 1")),
        Check("native-duplicate-first-wins", () => expectOutput(s,
          "nativelib.DupNative.which", """val res4: String = "first"""")),
        Check("native-missing-module-error", () => s.eval("nativelib.MissingNative.anything()").map { r =>
          check(!r.ok && r.error.exists(e =>
              e.contains("no JS module 'missingNative' registered") && e.contains("testNative")),
            s"got ok=${r.ok} error=${r.error}")
        }),
        Check("native-survives-reset", () => s.reset().flatMap(_ =>
          expectOutput(s, """nativelib.TestNative.greet("again")""",
            """val res0: String = "Hello from JS, again!""""))),
        Check("native-no-reload-on-reset", () => expectOutput(s,
          "nativelib.TestNative.totalLoads()", "val res1: Int = 1")),
      ))
    }

  // --- in-process fixture compiler ---------------------------------------------

  /** Batch-compiles virtual sources with `-scalajs` against in-memory classpath
   *  dirs, like [[JSReplDriver]] does for REPL lines. The output dir collects
   *  `.sjsir` from the SJS backend and `.tasty` from [[WriteFixtureTasty]]
   *  (GenBCode, which writes tasty on the JVM, is stubbed on Scala.js). */
  private class FixtureDriver(cps: List[VirtualDirectory]) extends dotc.Driver:
    override def sourcesRequired: Boolean = false

    override protected def initCtx: Context =
      val base = new ContextBase:
        override protected def newPlatform(using Context): Platform =
          new SJSPlatform:
            override def classPath(using Context): dotty.tools.io.ClassPath =
              AggregateClassPath(cps.map(VirtualDirectoryClassPath(_)))
      base.initialCtx

    def compileVirtual(sources: List[(String, String)]): Either[String, VirtualDirectory] =
      val outDir = new VirtualDirectory("(fixture-out)", None)
      setup(Array("-scalajs", "-color:never"), initCtx.fresh) match
        case None => Left("fixture compiler setup failed")
        case Some((_, ctx0)) =>
          val reporter = newStoreReporter
          val ctx = ctx0.fresh.setSetting(ctx0.settings.outputDir, outDir).setReporter(reporter)
          ctx.base.initialize()(using ctx)
          val run = FixtureCompiler().newRun(using ctx)
          run.compileSources(sources.map((name, content) => SourceFile.virtual(name, content)))
          val hadErrors = reporter.hasErrors
          val msgs = reporter.removeBufferedMessages(using ctx)
          if hadErrors then Left(msgs.map(_.msg.message).mkString("\n"))
          else Right(outDir)

  /** Plain compiler minus `JUnitBootstrappers` (crashes when JUnit is absent
   *  from the classpath, see [[JSReplCompiler]]) plus the tasty writer. */
  private class FixtureCompiler extends dotc.Compiler:
    override protected def transformPhases: List[List[Phase]] =
      val base = super.transformPhases.map(_.filterNot(_.isInstanceOf[dotc.transform.sjs.JUnitBootstrappers]))
      List(new WriteFixtureTasty) :: base

  /** Writes each top-level class's standard pickle (`unit.pickled`, populated
   *  by the `Pickler` phase just before `transformPhases`) to
   *  `<outputDir>/<package>/<Class>.tasty` — what a library jar would carry. */
  private class WriteFixtureTasty extends Phase:
    def phaseName: String = "writeFixtureTasty"
    override def isCheckable: Boolean = false
    protected def run(using Context): Unit =
      val unit = ctx.compilationUnit
      val outDir = ctx.settings.outputDir.value
      for (cls, bytes) <- unit.pickled do
        val parts = cls.fullName.mangledString.split('.').toList
        var dir: AbstractFile = outDir
        for p <- parts.init do dir = dir.subdirectoryNamed(p)
        val f = dir.fileNamed(parts.last.stripSuffix("$") + ".tasty")
        val os = f.output
        try os.write(bytes()) finally os.close()

  // --- helpers -----------------------------------------------------------------

  private final case class Check(name: String, body: () => Future[Boolean])

  private def runChecks(checks: List[Check]): Future[(Int, Int)] =
    seqFold(checks, (0, 0)) { case ((passed, failed), c) =>
      val fut = try c.body() catch case e: Throwable => Future.failed(e)
      fut.recover { case e: Throwable =>
        println(s"    exception: $e")
        false
      }.map { ok =>
        if ok then
          println(s"PASS extra-libs/${c.name}")
          (passed + 1, failed)
        else
          println(s"FAIL extra-libs/${c.name}")
          (passed, failed + 1)
      }
    }

  private def check(cond: Boolean, clue: => String): Boolean =
    if !cond then println(s"    $clue")
    cond

  private def expectOutput(s: ReplSession, code: String, expected: String): Future[Boolean] =
    s.eval(code).map { r =>
      check(r.ok && r.output.trim == expected,
        s"code: $code\n    expected: $expected\n    actual:   ${r.output.trim} (ok=${r.ok}, error=${r.error})")
    }

  /** Pack (path, content) entries into the `.bin` archive format — a test-local
   *  mirror of `writeBinArchive` in project/Build.scala. */
  private def mkArchive(entries: List[(String, Array[Byte])]): ArrayBuffer =
    var offset = 0
    val indexed = entries.map { (path, bytes) =>
      val e = (path, offset, bytes.length)
      offset += bytes.length
      e
    }
    val json = indexed.map((p, off, len) => s""""$p":[$off,$len]""").mkString("{", ",", "}")
    val encoder = js.Dynamic.newInstance(js.Dynamic.global.TextEncoder)()
    val jsonBytes = encoder.encode(json).asInstanceOf[Uint8Array]

    val buf = new ArrayBuffer(4 + jsonBytes.length + offset)
    new DataView(buf).setUint32(0, jsonBytes.length)
    new Uint8Array(buf).set(jsonBytes, 4)
    var pos = 4 + jsonBytes.length
    for (_, bytes) <- entries do
      val i8 = new Int8Array(buf, pos, bytes.length)
      var i = 0
      while i < bytes.length do
        i8(i) = bytes(i)
        i += 1
      pos += bytes.length
    buf

  private def utf8(s: String): Array[Byte] =
    val encoder = js.Dynamic.newInstance(js.Dynamic.global.TextEncoder)()
    val bytes = encoder.encode(s).asInstanceOf[Uint8Array]
    Array.tabulate(bytes.length)(i => bytes(i).toByte)

  /** Run `body` with stderr diverted, returning its value and what was written.
   *  Both routes are covered, as in `ReplSession.captureProcessOutput`. */
  private def captureStderr[A](body: => A): (A, String) =
    val out = new StringBuilder
    val console = js.Dynamic.global.console
    val errObj = js.Dynamic.global.process.stderr
    val oldError = console.error
    val oldWrite = errObj.write
    console.updateDynamic("error")(((chunk: js.Any) =>
      out.append(chunk.toString).append("\n")
      ()
    ): js.Function1[js.Any, Unit])
    errObj.updateDynamic("write")(((chunk: js.Any) =>
      out.append(chunk.toString)
      true
    ): js.Function1[js.Any, Boolean])
    try (body, out.toString)
    finally
      console.updateDynamic("error")(oldError)
      errObj.updateDynamic("write")(oldWrite)

  /** Write a buffer to a fresh temp file, returning its path. */
  private def writeTempFile(name: String, buffer: ArrayBuffer): String =
    val fs = js.Dynamic.global.require("fs")
    val os = js.Dynamic.global.require("os")
    val path = js.Dynamic.global.require("path")
    val dir = fs.mkdtempSync(path.join(os.tmpdir(), "extra-libs-test-")).asInstanceOf[String]
    val p = path.join(dir, name).asInstanceOf[String]
    fs.writeFileSync(p, new Uint8Array(buffer))
    p

  private def seqFold[A, B](xs: List[A], acc: B)(f: (B, A) => Future[B]): Future[B] =
    xs match
      case Nil     => Future.successful(acc)
      case h :: tl => f(acc, h).flatMap(acc2 => seqFold(tl, acc2)(f))
