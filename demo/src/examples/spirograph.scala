import scala.scalajs.js

case class SpiroCurve(outerR: Double, innerR: Double, pen: Double, hue: Int)

object Spirograph:
  def gcd(a: Long, b: Long): Long =
    if b == 0 then a else gcd(b, a % b)

  def trace(curve: SpiroCurve, steps: Int): Seq[(Double, Double)] =
    val SpiroCurve(bigR, r, d, _) = curve
    val period = gcd(bigR.toLong, r.toLong)
    (0 to steps).map { i =>
      val t = i * Math.PI * 2 * r / period / steps
      val x = (bigR - r) * Math.cos(t) + d * Math.cos((bigR - r) / r * t)
      val y = (bigR - r) * Math.sin(t) - d * Math.sin((bigR - r) / r * t)
      (x, y)
    }

  def draw(ctx: js.Dynamic, cx: Double, cy: Double, curve: SpiroCurve, steps: Int): Unit =
    val points = trace(curve, steps)
    ctx.beginPath()
    points.zipWithIndex.foreach { (pt, i) =>
      val (x, y) = pt
      if i == 0 then ctx.moveTo(cx + x, cy + y)
      else ctx.lineTo(cx + x, cy + y)
    }
    ctx.strokeStyle = s"hsla(${curve.hue}, 75%, 50%, 0.75)"
    ctx.lineWidth = 1.2
    ctx.stroke()

val curves = List(
  SpiroCurve(outerR = 150, innerR = 47, pen = 110, hue = 220),
  SpiroCurve(outerR = 150, innerR = 83, pen = 130, hue = 340),
  SpiroCurve(outerR = 150, innerR = 61, pen = 100, hue = 130),
)

@main def main() =
  val canvas = js.Dynamic.global.document.getElementById("appCanvas")
  canvas.style.display = "block"
  canvas.width = 600
  canvas.height = 600
  val ctx = canvas.getContext("2d")

  ctx.fillStyle = "#fafaf9"
  ctx.fillRect(0, 0, 600, 600)

  val cx = 300.0
  val cy = 300.0
  curves.foreach(Spirograph.draw(ctx, cx, cy, _, steps = 3000))

  ctx.fillStyle = "#111827"
  ctx.font = "bold 18px monospace"
  ctx.textAlign = "center"
  ctx.fillText("Spirograph", 300, 28)
  ctx.font = "12px monospace"
  ctx.fillStyle = "#6b7280"
  ctx.fillText(s"${curves.length} parametric curves, 3000 steps each", 300, 46)

  println("Spirograph rendered!")
