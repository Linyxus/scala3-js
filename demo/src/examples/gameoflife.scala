import scala.scalajs.js

class Grid(val rows: Int, val cols: Int, cells: Array[Array[Boolean]]):
  def alive(r: Int, c: Int): Boolean = cells(r)(c)

  def countNeighbors(r: Int, c: Int): Int =
    var count: Int = 0
    for
      dr <- -1 to 1
      dc <- -1 to 1
      if !(dr == 0 && dc == 0)
      if cells((r + dr + rows) % rows)((c + dc + cols) % cols)
    do count += 1
    count

  def step: Grid =
    val next = Array.tabulate(rows, cols) { (r, c) =>
      val n = countNeighbors(r, c)
      if alive(r, c) then n == 2 || n == 3 else n == 3
    }
    Grid(rows, cols, next)

  def population: Int = cells.flatten.count(identity)

object Grid:
  def random(rows: Int, cols: Int, density: Double, seed: Int): Grid =
    val rng = scala.util.Random(seed)
    val cells = Array.tabulate(rows, cols)((_, _) => rng.nextDouble() < density)
    Grid(rows, cols, cells)

class Simulation(initial: Grid, generations: Int):
  val (finalGrid, ages) = run()

  private def run(): (Grid, Array[Array[Int]]) =
    val cellAges = Array.fill(initial.rows, initial.cols)(0: Int)
    var grid = initial
    for _ <- 0 until generations do
      grid = grid.step
      for r <- 0 until grid.rows; c <- 0 until grid.cols do
        if grid.alive(r, c) then cellAges(r)(c) += 1
        else cellAges(r)(c) = 0
    (grid, cellAges)

object LifeRenderer:
  def draw(ctx: js.Dynamic, sim: Simulation, cellSize: Int, w: Int, h: Int): Unit =
    ctx.fillStyle = "#fafaf9"
    ctx.fillRect(0, 0, w, h)

    val grid = sim.finalGrid
    for r <- 0 until grid.rows; c <- 0 until grid.cols do
      if grid.alive(r, c) then
        val age = sim.ages(r)(c).min(50)
        val hue = 220 + age * 2
        val light = 55 - (age * 0.4).toInt.min(20)
        ctx.fillStyle = s"hsl($hue, 75%, $light%)"
        ctx.fillRect(c * cellSize, r * cellSize, cellSize - 1, cellSize - 1)

    ctx.fillStyle = "#111827"
    ctx.font = "bold 16px monospace"
    ctx.textAlign = "center"
    ctx.fillText(s"Game of Life — ${sim.finalGrid.population} live cells", w / 2, 20)

@main def main() =
  val w = 600
  val h = 400
  val cellSize = 4
  val generations = 200

  val canvas = js.Dynamic.global.document.getElementById("appCanvas")
  canvas.style.display = "block"
  canvas.width = w
  canvas.height = h
  val ctx = canvas.getContext("2d")

  val initial = Grid.random(h / cellSize, w / cellSize, density = 0.35, seed = 12345)
  val sim = Simulation(initial, generations)
  LifeRenderer.draw(ctx, sim, cellSize, w, h)

  val grid = sim.finalGrid
  println(s"Game of Life: ${grid.cols}x${grid.rows} grid, $generations generations")
  println(s"Live cells: ${grid.population} / ${grid.rows * grid.cols}")
