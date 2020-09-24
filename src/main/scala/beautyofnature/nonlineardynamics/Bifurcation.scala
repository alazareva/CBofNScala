package beautyofnature.nonlineardynamics

import processing.core.{PApplet, PConstants}


case class BifurcationConfig(rMin: Float, rMax: Float, skip: Int, f: (Float, Float) => Float)

object Bifurcations {

  val logisticMap = BifurcationConfig(
    0.00f,
    1.0f,
    500,
    (x, r) => 4 * r * x * (1 - x)
  )
}


class Bifurcation extends PApplet {

  val config: BifurcationConfig = Bifurcations.logisticMap

  val tol: Float = 0.01f / height

  val rStep = 0.05f

  override def settings(): Unit = {
    size(800, 500, PConstants.P2D)
  }

  def isPeriodic(prevX: List[Float], x: Float): Boolean = {
    prevX.exists(p => Math.abs(p - x) < tol)
  }

  override def draw(): Unit = {
    background(255)
    (0 until width).foreach { i =>
      val r = PApplet.map(i, 0, width, config.rMin + frameCount * rStep, config.rMax)

      val xInitial = (0 until config.skip).foldLeft(0.5f)((x, _) => config.f(x, r))

      def rec(j: Int, x: Float, prevX: List[Float]): Unit = {
        if (j >= height * 2 || isPeriodic(prevX, x)) ()
        else {
          point(i, x * height)
          rec(j + 1, config.f(x, r), x :: prevX.dropRight(1))
        }
      }

      rec(0, xInitial, List(2.0f, 3.0f, 4.0f, 5, 0f))
    }
    if (config.rMin + (frameCount + 1) * rStep >= config.rMax) noLoop()
  }

  override def setup(): Unit = {
    background(255)
    colorMode(PConstants.RGB, 255, 255, 255, 100)
  }

}


object Bifurcation extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.nonlineardynamics.Bifurcation")
  }
}