package beautyofnature.cellularautomata

import processing.core.{PApplet, PConstants}

import scala.util.Random

case class GameOfLifeCell(state: Int, sum: Int = 0, newSum: Int = 0)


class GameOfLife extends PApplet {

  val w = 3
  val h = 3
  val wid = 400
  val high = 400
  val extra = 5
  val rw = Math.max(w + extra * 2, wid)
  val rh = Math.max(h + extra * 2, high)
  val prodOfInitialAlive = 0.15f

  def randomCell: GameOfLifeCell =  if (Random.nextFloat() < prodOfInitialAlive) GameOfLifeCell(1) else GameOfLifeCell(0)
  val cells: Array[Array[GameOfLifeCell]] = Array.fill(rh, rw)(randomCell)

  override def settings(): Unit = {
    size(rw, rh, PConstants.P2D)
  }

  def updateSum(rw: Int, rh: Int, ii: Int, jj: Int, change: Int): Unit = {
    for {
      k <- -1 to 1
      l <- -1 to 1
      if k != 0 || l !=0
    } {
      val j = (k + jj + rh) % rh
      val i = (l + ii + rw) % rw
      val cell = cells(j)(i)
      cells(j)(i) = cell.copy(newSum = cell.newSum + change)
    }
  }

  def copySum(): Unit = {
    for {
      i <- 0 until rw
      j <- 0 until rh
    } {
      val cell = cells(j)(i)
      cells(j)(i) = cell.copy(sum = cell.newSum)
    }
  }

  override def setup(): Unit = {
    strokeWeight(3)
    background(255)
    for {
      i <- 0 until rw
      j <- 0 until rh
    } {
      if (cells(j)(i).state == 1) updateSum(rw, rh, i, j, 1)
    }
  }

  override def draw(): Unit = {
    background(255)
    copySum()
    for {
      i <- 0 until rw
      j <- 0 until rh
    } {
      val cell = cells(j)(i)
      if (cell.state == 0 && cell.sum == 3) {
        updateSum(rw, rh, i, j, 1)
        cells(j)(i) = cell.copy(state = 1)
        stroke(0)
        point(i, j)
      } else if (cell.state == 1 && (cell.sum < 2 || cell.sum >  3)) {
        updateSum(rw, rh, i, j, -1)
        cells(j)(i) = cell.copy(state = 0)
        stroke(255)
        point(i, j)
      }
    }
  }
}


object GameOfLife extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.cellularautomata.GameOfLife")
  }
}


