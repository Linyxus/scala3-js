@main def main() =
  println("Hello from Scala.js in the browser!")
  println(s"2 + 2 = ${2 + 2}")

  val fruits = List("apple", "banana", "cherry")
  val upper = fruits.map(_.toUpperCase)
  println(s"Fruits: $upper")

  val fib = LazyList.iterate((0, 1)) { (a, b) => (b, a + b) }.map(_(0))
  println(s"Fibonacci: ${fib.take(12).toList}")
