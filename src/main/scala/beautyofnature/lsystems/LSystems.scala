package beautyofnature.lsystems
import processing.core.{PApplet, PConstants}

class LSystems extends PApplet {

  override def settings(): Unit = {
    size(500, 500, PConstants.P2D)
  }

  override def setup(): Unit = {
    colorMode(PConstants.HSB, 360)
    background(360)
    noLoop()
  }

  override def draw(): Unit = {
    (1 to 20).foreach(i => circle(i * 15, 10, 50))
  }

}

object LSystems extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.lsystems.LSystems")
  }
}