package beautyofnature.mandelbrotAndJulia

import processing.core.{PApplet, PConstants}

import scala.annotation.tailrec


case class Point(x: Float, y: Float)

case class Extent(xStart: Float, xEnd: Float, yStart: Float, yEnd: Float) {
  def *(m: Float): Extent = Extent(xStart * m, xEnd * m, yStart * m, yEnd * m)

  def +(v: Point): Extent = Extent(xStart + v.x, xEnd + v.x, yStart + v.y, yEnd + v.y)
}

object Functions {

  def mandelbrot(maxIterations: Int = 1000, escape: Int = 2)(initialZ: ComplexNumber): Int = {
    val f = (c1: ComplexNumber, c2: ComplexNumber) => c1.pow(2) + c2

    @tailrec
    def rec(z1: ComplexNumber, iteration: Int): Int = {
      if (z1.abs >= escape || iteration >= maxIterations) iteration
      else rec(f(z1, initialZ), iteration + 1)
    }

    rec(ComplexNumber(0, 0), 0)
  }

  def julia(maxIterations: Int=1000, maxZ: Int=10)(initialZ: ComplexNumber, c: ComplexNumber): Int =  {
    val f = (z: ComplexNumber) => z.pow(2) + c
    @tailrec
    def rec(z: ComplexNumber, iteration: Int): Int = {
      if (z.abs >= maxZ || iteration > maxIterations) iteration
      else rec(f(z), iteration + 1)
    }
    rec(initialZ, 0)
  }
}


class MandelbrotAndJulia extends PApplet {

  val juliaExtent = Extent(-2f, 2f, -2f, 2f)
  var extent = juliaExtent
  val maxIterations = 100
  var juliaCoordinate: Option[ComplexNumber] = None
  var boxStart: Option[ComplexNumber] = None
  var boxEnd: Option[ComplexNumber] = None

  override def settings(): Unit = {
    size(1000, 500, PConstants.P2D)
  }

  def getCoordinate(x: Int, y: Int, extent: Extent): ComplexNumber = {
    val xPos = PApplet.map(x, 0, width / 2, extent.xStart, extent.xEnd)
    val yPos = PApplet.map(y, 0, height, extent.yStart, extent.yEnd)
    ComplexNumber(xPos, yPos)
  }

  def getRealCoordinate(c: ComplexNumber, extent: Extent): (Float, Float) = {
    val xPos = PApplet.map(c.r.toFloat, extent.xStart, extent.xEnd, 0, width / 2)
    val yPos = PApplet.map(c.i.toFloat, extent.yStart, extent.yEnd, 0, height)
    (xPos, yPos)
  }

  def drawMandelbrot(): Unit = {
    //fill(255)
    //rect(0, 0, width/2, height)
    println("Drawing mandelbrot")
    for (x <- 0 until width / 2; y <- 0 until height) {
      val z = getCoordinate(x, y, extent)
      val iterations = Functions.mandelbrot(maxIterations)(z)
      val a = PApplet.map(iterations, 0, maxIterations, 255, 0)
      val col = color(a)
      set(x, y, col)
      /*
      if (iterations >= maxIterations) {
        val c = color(0)
        set(x, y, c)
      } */
    }
    juliaCoordinate match {
      case Some(c) => {
        val (x, y) = getRealCoordinate(c, extent)
        stroke(200, 0, 0)
        strokeWeight(1)
        noFill()
        circle(x, y, 10)
      }
      case _ => ()
    }
    }

  def drawJulia(): Unit = {
   // fill(255)
    //rect(width/2, 0, width, height)
    juliaCoordinate match {
      case None => ()
      case Some(c) =>
        println("Drawing julia")
        for (x <- width / 2 until width; y <- 0 until height) {
          val z = getCoordinate(x - width / 2, y, juliaExtent)
          val iterations = Functions.julia(maxIterations)(z, c)
          val a = PApplet.map(iterations, 0, maxIterations, 255, 0)
          val col = color(a)
          set(x, y, col)
          if (iterations >= maxIterations) {
            val c = color(0)
            //set(x, y, c)
          }
        }
    }
  }

  override def setup(): Unit = {
    background(255)
    colorMode(PConstants.RGB, 255, 255, 255, 100)
  }

  override def draw(): Unit = {
    colorMode(PConstants.RGB, 255, 255, 255, 100)
    background(255)
    drawMandelbrot()
    drawJulia()
    noLoop()
  }

  override def mouseClicked(): Unit = {
    juliaCoordinate = Some(getCoordinate(mouseX,  mouseY, extent))
    redraw()
  }

  override def mousePressed(): Unit = {
    if (mouseX < width / 2) {
      boxStart = Some(getCoordinate(mouseX, mouseY, extent))
  }}

  override def mouseDragged(): Unit = {
    if (mouseX < width / 2) {
      boxEnd = Some(getCoordinate(mouseX, mouseY, extent))
    }}

  override def keyPressed(): Unit = {
    if (key == 'r') {
      extent = juliaExtent
      redraw()
    }
  }

  override def mouseReleased(): Unit = {
    if (mouseX < width / 2 ) (boxStart, boxEnd) match {
      case (Some(ComplexNumber(startx, starty)), Some(ComplexNumber(endx, endy))) =>
      // TODO handle dragging in other directions
        println("Setting extent")
        val startxf = startx.toFloat
        val startyf = starty.toFloat
        val maxSize = Math.max(endx - startxf, endy - startyf).toFloat
        extent = Extent(startxf, startxf + maxSize, startyf, startyf + maxSize)
        juliaCoordinate = None
        println(extent)
        redraw()
      case _ => ()
    }
    boxStart = None
    boxEnd = None
  }

}

object MandelbrotAndJulia extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.mandelbrotAndJulia.MandelbrotAndJulia")
  }
}