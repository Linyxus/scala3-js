import scala.scalajs.js
import scala.util.Random

case class Circle(x: Int, y: Int, radius: Int, hue: Int)

object CircleField:
  def generate(width: Int, height: Int, count: Int, seed: Int): Vector[Circle] =
    val rng = Random(seed)
    Vector.tabulate(count) { i =>
      Circle(
        x = rng.nextInt(width),
        y = rng.nextInt(height),
        radius = 12 + rng.nextInt(36),
        hue = (i * 137) % 360
      )
    }

  def nearbyPairs(circles: Vector[Circle], maxDist: Double): Seq[(Circle, Circle)] =
    for
      i <- circles.indices
      j <- (i + 1) until circles.length
      a = circles(i)
      b = circles(j)
      if Math.hypot(b.x - a.x, b.y - a.y) < maxDist
    yield (a, b)

object Renderer:
  def drawBackground(ctx: js.Dynamic, w: Int, h: Int): Unit =
    ctx.fillStyle = "#ffffff"
    ctx.fillRect(0, 0, w, h)

  def drawCircle(ctx: js.Dynamic, c: Circle): Unit =
    ctx.beginPath()
    ctx.arc(c.x, c.y, c.radius, 0, Math.PI * 2)
    ctx.fillStyle = s"hsla(${c.hue}, 75%, 60%, 0.55)"
    ctx.fill()

  def drawConnection(ctx: js.Dynamic, a: Circle, b: Circle): Unit =
    ctx.beginPath()
    ctx.moveTo(a.x, a.y)
    ctx.lineTo(b.x, b.y)
    ctx.stroke()

  def drawTitle(ctx: js.Dynamic, w: Int, title: String, subtitle: String): Unit =
    ctx.fillStyle = "#111827"
    ctx.font = "bold 22px monospace"
    ctx.textAlign = "center"
    ctx.fillText(title, w / 2, 30)
    ctx.font = "14px monospace"
    ctx.fillStyle = "#6b7280"
    ctx.fillText(subtitle, w / 2, 52)

@main def main() =
  val canvas = js.Dynamic.global.document.getElementById("appCanvas")
  canvas.style.display = "block"
  val ctx = canvas.getContext("2d")
  val w = canvas.width.asInstanceOf[Int]
  val h = canvas.height.asInstanceOf[Int]

  val circles = CircleField.generate(w, h, count = 60, seed = 42)
  val connections = CircleField.nearbyPairs(circles, maxDist = 120)

  Renderer.drawBackground(ctx, w, h)
  circles.foreach(Renderer.drawCircle(ctx, _))

  ctx.strokeStyle = "rgba(17, 24, 39, 0.08)"
  ctx.lineWidth = 1
  connections.foreach((a, b) => Renderer.drawConnection(ctx, a, b))

  Renderer.drawTitle(ctx, w, "Scala 3 + Scala.js", "Compiled and linked in the browser")
  println(s"Drew ${circles.length} circles with ${connections.length} connections")
