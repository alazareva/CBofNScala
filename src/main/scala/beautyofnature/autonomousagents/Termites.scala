package beautyofnature.autonomousagents

import scala.util.Random
import processing.core.{PApplet, PConstants}

trait Direction {
  def X: Int
  def Y: Int
}

case object N extends Direction {
  val X = 0
  val Y = 1
}

case object NE extends Direction {
  val X = 1
  val Y = 1
}

case object E extends Direction {
  val X = 1
  val Y = 0
}

case object SE extends Direction {
  val X = 1
  val Y = -1
}

case object S extends Direction {
  val X = 0
  val Y = -1
}

case object SW extends Direction {
  val X = -1
  val Y = -1
}

case object W extends Direction {
  val X = -1
  val Y = 0
}

case object NW extends Direction {
  val X = -1
  val Y = 1
}

object Directions {
  val directions = List(N, NE, E, SE, S, SW, W, NW)
  def apply(i: Int): Direction = directions(i)
}

case class Termite(x: Int, y: Int, direction: Int)


class Termites extends PApplet {

  val w = 250
  val h = 250
  val num = 10000
  val chipProbability = 0.3

  def randomChip: Int = if (Random.nextFloat() < chipProbability) 1 else 0

  val chips: Array[Array[Int]] = Array.fill(h, w)(randomChip)
  def random: Termite = Termite(Random.nextInt(w), Random.nextInt(h), Random.nextInt(8))
  val termites: Array[Termite] = Array.fill(num)(random)

  override def settings(): Unit = {
    size(w, h, PConstants.P2D)
  }


  def turnAndStep(x: Int, y: Int, newDirection: Int): (Int, Int, Int) = {
    val direction = Directions(newDirection)
    ((x + direction.X + w) % w, (y + direction.Y + h) % h, newDirection)
  }

  def newStep(x: Int, y: Int, d: Int): (Int, Int, Int) = {
    val newDirection = (d + ((Random.nextInt % 3) - 1) + 8) % 8
    turnAndStep(x, y, newDirection)
  }

  def turnAround(x: Int, y: Int, d: Int): (Int, Int, Int) = {
    val newDirection = (d + 4) % 8
    turnAndStep(x, y, newDirection)
  }

  def hasWood(x: Int, y: Int): Boolean = chips(y)(x) == 1

  def update(): Unit = {

    termites.indices.foreach(i => {
      val Termite(x, y, d) = termites(i)
      val (nx, ny, nd) = newStep(x, y, d)
      val updatedTermite = {
        if (hasWood(x, y) && !hasWood(nx, ny)) {
          chips(y)(x) = 0
          chips(ny)(nx) = 1
          Termite(nx, ny, nd)
        } else if (hasWood(x, y) && hasWood(nx, ny)) {
          val (nnx, nny, nnd) = turnAround(x, y, d)
          Termite(nnx, nny, nnd)
        } else {
          Termite(nx, ny, nd)
        }
      }
      termites(i) = updatedTermite
    })

  }

  override def draw(): Unit = {
    update()
    for {
      i <- 0 until w
      j <- 0 until h
    } {
      val c = if (chips(j)(i) == 1) color(0) else color(255)
      stroke(c)
      point(i, j)
    }
  }
}

object Termites extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.autonomousagents.Termites")
  }
}
