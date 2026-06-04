package java.lang.ref

class ReferenceQueue[T] {
  def poll(): Reference[? <: T] | Null = null
  def remove(timeout: Long): Reference[? <: T] | Null = null
  def remove(): Reference[? <: T] | Null = null
}
