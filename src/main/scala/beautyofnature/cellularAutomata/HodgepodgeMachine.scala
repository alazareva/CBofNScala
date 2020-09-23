package beautyofnature.cellularAutomata

import processing.core.{PApplet, PConstants}

import scala.util.Random

class HodgepodgeMachine extends PApplet {

  val frequency = 1
  val states = 100

  val w = 500
  val h = 500

  var oldState = Array.fill(h, w)(Random.nextInt(states))
  var newState = Array.fill(h, w)(0)

  val k1 = 2
  val k2 = 3
  val gg = 34


  override def settings(): Unit = {
    size(w, h, PConstants.P2D)
  }

  def updateCell(i: Int, j: Int) = {
    var sum = oldState(j)(i)
    var numinf = 0
    var numill = 0
    for {
      x <- -1 to 1
      y <- -1 to 1
      if !(x == 0 && y == 0)
    } {
      val nx = x + i
      val ny = y + j
      // TODO handle wrap
      if (nx >= 0 && nx < width && ny >= 0 && ny < height) {
        sum += oldState(ny)(nx)
        if (oldState(ny)(nx) == states - 1) numill += 1
        else if (oldState(ny)(nx) > 0) numinf += 1
      }
    }
    if (oldState(j)(i) == 0) newState(j)(i) = (Math.floor(numinf / k1) + Math.floor(numill / k2)).toInt
    else if (oldState(j)(i) < states - 1) newState(j)(i) = Math.floor(sum / (numinf + 1)).toInt + gg
    else newState(j)(i) = 0

    if (newState(j)(i) > states - 1) newState(j)(i) = states - 1
  }

  override def draw(): Unit = {
    for {
      i <- 0 until width
      j <- 0 until height
    } {
      updateCell(i, j)
      if (frameCount % frequency == 0) {
        stroke(PApplet.map(oldState(j)(i), 0, states, 0, 255))
        point(i, j)
      }
    }
    val swap = oldState
    oldState = newState
    newState = swap
  }
}


object HodgepodgeMachine extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.cellularAutomata.HodgepodgeMachine")
  }
}


