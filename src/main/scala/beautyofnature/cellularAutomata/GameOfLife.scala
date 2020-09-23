package beautyofnature.cellularAutomata

import processing.core.{PApplet, PConstants}

import scala.util.Random


class GameOfLife extends PApplet {

  val w = 3
  val h = 3
  val wid = 400
  val high = 400
  val extra = 5
  val rw = Math.max(w + extra * 2, wid)
  val rh = Math.max(h + extra * 2, high)
  val prodOfInitialAlive = 0.15f


  var sum = Array.fill(rh, rw)(0)
  val state = Array.fill(rh, rw)(0)
  var newSum = Array.fill(rh, rw)(0)

  override def settings(): Unit = {
    size(rw, rh, PConstants.P2D)
  }

  override def setup(): Unit = {
    strokeWeight(3)
    background(255)
    for {
      i <- 0 until rw
      j <- 0 until rh
    } {
      val data = if (Random.nextFloat() < prodOfInitialAlive) 1 else 0
      state(j)(i) = data
      if (data == 1) {
        updateCount(sum, rw, rh, i, j, 1)
      }
      val c = if (data == 1) color(0) else color(255)
      stroke(c)
      point(i, j)
    }
  }


  def updateCount(arr: Array[Array[Int]], rw: Int, rh: Int, ii: Int, jj: Int, change: Int): Unit = {
    for {
      i <- -1 to 1
      j <- -1 to 1
      if i != 0 || j !=0
    } {
      arr((j + jj + rh) % rh)((i + ii + rw) % rw) += change
    }
  }

  def copyOver(): Unit = {
    for {
      i <- 0 until rw
      j <- 0 until rh
    } newSum(j)(i) = sum(j)(i)
  }

  override def draw(): Unit = {
    background(255)
    copyOver()
    for {
      i <- 0 until rw
      j <- 0 until rh
    } {
      if (state(j)(i) == 0 && sum(j)(i) == 3) {
        updateCount(newSum, rw, rh, i, j, 1)
        state(j)(i) = 1
        stroke(0)
        point(i, j)
      } else if (state(j)(i) == 1 && (sum(j)(i) < 2 || sum(j)(i) > 3)) {
        updateCount(newSum, rw, rh, i, j, -1)
        state(j)(i) = 0
        stroke(255)
        point(i, j)
      }
    }
    val swap = sum
    sum = newSum
    newSum = swap
  }
}


object GameOfLife extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.cellularAutomata.GameOfLife")
  }
}


