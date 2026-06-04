package dotty.tools.dotc.profile

import java.util.concurrent.*
import dotty.tools.dotc.core.Phases.Phase

/** JS override of upstream `ProfiledThreadPool.scala`.
 *
 *  Upstream's version additionally defines a `RealProfiler`-based
 *  `ProfilingThreadPoolFactory`, but the JS build has no `RealProfiler`
 *  (profiling is stubbed to `NoOpProfiler` in `Profiler.scala`) and never runs
 *  parallel backend codegen: the sole caller, `backend/jvm/GeneratedClassHandler`,
 *  is JVM-only and excluded from the Scala.js source set. So the profiling
 *  factory and its `ThreadGroup`/`ThreadFactory` machinery (unsupported on
 *  Scala.js) are dropped here.
 *
 *  This override also subsumes the old `ThreadPoolFactory.scala`, which upstream
 *  deleted when merging it into this file. Nothing in the JS source set actually
 *  invokes any of this; it only needs to compile.
 */
object ProfiledThreadPool {
  def newExecutor(phase: Phase, profiler: Profiler, nThreads: Int, maxQueueSize: Int, shortId: String): ThreadPoolExecutor =
    new ThreadPoolFactory(phase).newBoundedQueueFixedThreadPool(nThreads, maxQueueSize, shortId)
}

private[profile] class ThreadPoolFactory(phase: Phase) {
  def newBoundedQueueFixedThreadPool(nThreads: Int, maxQueueSize: Int, shortId: String): ThreadPoolExecutor =
    // Like Executors.newFixedThreadPool; no custom ThreadGroup/ThreadFactory
    // (those are unsupported on Scala.js and this is never executed anyway).
    new ThreadPoolExecutor(nThreads, nThreads, 0L, TimeUnit.MILLISECONDS, new ArrayBlockingQueue[Runnable](maxQueueSize))
}
