package beautyofnature.cellularautomata

import processing.core.{PApplet, PConstants}

import scala.util.Random

case class Cell(oldState: Int, newState: Int = 0)

class HodgepodgeMachine extends PApplet {

  val frequency = 1
  val states = 100

  val w = 500
  val h = 500

  val cells: Array[Array[Cell]] = Array.fill(h, w)(Cell(Random.nextInt(states)))

  val k1 = 2
  val k2 = 3
  val gg = 34


  override def settings(): Unit = {
    size(w, h, PConstants.P2D)
  }

  def updateCell(i: Int, j: Int): Unit = {
    val cell = cells(j)(i)
    var sum = cell.oldState
    var numInf = 0
    var numIll = 0
    for {
      x <- -1 to 1
      y <- -1 to 1
      if !(x == 0 && y == 0)
    } {
      val nx = x + i
      val ny = y + j
      if (nx >= 0 && nx < width && ny >= 0 && ny < height) {
        sum += cells(ny)(nx).oldState
        if (cells(ny)(nx).oldState == states - 1) numIll += 1
        else if (cells(ny)(nx).oldState > 0) numInf += 1
      }
    }

    val newState = {
      val candidate = {
        if (cell.oldState == 0) Math.floor(numInf / k1) + Math.floor(numIll / k2)
        else if (cell.oldState < states - 1) Math.floor(sum / (numInf + 1)).toInt + gg
        else 0
      }
      if (candidate > states - 1) states - 1 else candidate.toInt
    }
    cells(j)(i) = cell.copy(newState = newState)
  }

  def swapOldAndNew(): Unit = {
    for {
      i <- 0 until width
      j <- 0 until height
    } {
    val cell = cells(j)(i)
      cells(j)(i) = cell.copy(oldState = cell.newState, newState = cell.oldState)
    }
  }

  override def draw(): Unit = {
    for {
      i <- 0 until width
      j <- 0 until height
    } {
      updateCell(i, j)
      if (frameCount % frequency == 0) {
        stroke(PApplet.map(cells(j)(i).oldState, 0, states, 0, 255))
        point(i, j)
      }
    }
    swapOldAndNew()
  }
}


object HodgepodgeMachine extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.cellularautomata.HodgepodgeMachine")
  }
}


