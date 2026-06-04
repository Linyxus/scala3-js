package java.lang.ref

abstract class Reference[T] {
  // The real `java.lang.ref.Reference.get()` is a Java method, so under explicit
  // nulls it yields a flexible type usable as both `T` and `T | Null`. We can't
  // express flexible types from Scala source, so we return non-null `T`: shared
  // callers (e.g. WeakHashSet) use the result both null-checked and as a value,
  // and in this JS port references are never actually cleared/GC'd (see
  // WeakReference's stubbed enqueue/isEnqueued), so `get` never returns null.
  def get: T
  def clear(): Unit
  def isEnqueued: Boolean
  def enqueue(): Boolean
}
