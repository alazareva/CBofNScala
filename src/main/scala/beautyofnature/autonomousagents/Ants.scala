package beautyofnature.autonomousagents

import processing.core.{PApplet, PConstants}

import scala.util.Random

case class Ant(x: Int, y: Int, dir: Int)

class Ants extends PApplet {
  val w = 200
  val h = 200
  val num = 2
  val p = 0.0
  val rule = "10"
  val states = rule.distinct.length

  def randomAnt = Ant(Random.nextInt(w), Random.nextInt(h), Random.nextInt(4))

  val ants = Array.fill(num)(randomAnt)

  def randomCrumb: Int = if (Random.nextFloat < p) Random.nextInt(states) else 0

  val grid = Array.fill(h, w)(randomCrumb)

  override def settings(): Unit = {
    size(w, h, PConstants.P2D)
  }

  def wrap(v: Int, s: Int): Int = {
    if (v < 0) s - 1
    else if (v > s - 1) 0
    else v
  }

  def newPosition(ant: Ant): (Int, Int) = {
    val (nx, ny) = ant.dir match {
      case 0 => (ant.x, ant.y + 1)
      case 1 => (ant.x + 1, ant.y)
      case 2 => (ant.x, ant.y - 1)
      case _  => (ant.x - 1, ant.y)
    }
    (wrap(nx, w), wrap(ny, h))
  }

  def newDirection(oldDir: Int, oldCrumb: Int): Int = {
    val d = if (rule(oldCrumb) - '0' == 0) 5 else 3
    (oldDir + d) % 4
  }

  def update(): Unit = {
    ants.indices.foreach { i => {
      val current = ants(i)
      val (nx, ny) = newPosition(current)
      val oldCrumb = grid(ny)(nx)
      grid(ny)(nx) = (oldCrumb + 1) % states
      ants(i) = Ant(nx, ny, newDirection(current.dir, oldCrumb))
    }}
  }

  override def setup(): Unit = {
    strokeWeight(1)
  }

  def update(n: Int): Unit = {
    (0 until n).foreach(_ => update())
  }

  override def draw(): Unit = {
    update(10)
    for {
      i <- 0 until w
      j <- 0 until h
    } {
      val c = PApplet.map(grid(j)(i), 0, states, 255, 0)
      stroke(c)
      point(i, j)
    }
  }
}

object Ants extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.autonomousagents.Ants")
  }
}
