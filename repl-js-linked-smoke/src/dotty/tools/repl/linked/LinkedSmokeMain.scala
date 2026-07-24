package dotty.tools
package repl
package linked

/** Single main-module entry point for the linked smoke project, dispatching to the
 *  stage-A pipeline smoke ([[LinkedImageSmoke]]) or the stage-B functional test
 *  ([[LinkedModeTest]]) by CLI arg — so both share one linked `main.js`. The sbt
 *  tasks `linkedImageSmoke` (no arg) and `linkedModeTest` (`mode-test`) select. */
object LinkedSmokeMain:
  def main(args: Array[String]): Unit =
    if ReplBootstrap.args.contains("mode-test") then LinkedModeTest.main(args)
    else LinkedImageSmoke.main(args)
