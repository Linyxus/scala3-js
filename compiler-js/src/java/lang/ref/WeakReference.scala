package java.lang.ref

class WeakReference[T](referent: T, queue: ReferenceQueue[? >: T] | Null) extends Reference[T] {
  def this(referent: T) = this(referent, null)

  private var ref: T | Null = referent

  // Returns non-null `T` to mirror the real JDK's flexible-typed `get()` (see
  // Reference). `ref` is only nulled by `clear()`, which is never driven in this
  // JS port (no GC/ReferenceQueue integration), so `.nn` never throws here.
  def get: T = ref.nn
  def clear(): Unit = ref = null
  def isEnqueued: Boolean = false
  def enqueue(): Boolean = false
}
