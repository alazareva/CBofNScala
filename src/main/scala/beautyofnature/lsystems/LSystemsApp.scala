package beautyofnature.lsystems

import processing.core.{PApplet, PConstants}


case class LSystem(sentence: String, rules: Map[Char, String], generation: Int = 0) {

  def next: LSystem = LSystem(sentence.flatMap(c => rules.getOrElse(c, c.toString)), rules, generation + 1)

}

case class TurtleInfo(len: Float,
                      theta: Float,
                      xStart: Float,
                      yStart: Float,
                      thetaStart: Float,
                      scalingFactor: Float = 0.5f
                     )

class Turtle(val turtleInfo: TurtleInfo, var lSystem: LSystem) {

  def redraw(applet: PApplet): Unit = {
    if (lSystem.generation < 7) {
      lSystem = lSystem.next
      applet.redraw()
    }
  }

  def render(applet: PApplet): Unit = {
    applet.pushMatrix()
    applet.translate(turtleInfo.xStart, turtleInfo.yStart)
    applet.rotate(turtleInfo.thetaStart)
    lSystem.sentence.foreach({
      case '[' => applet.pushMatrix()
      case ']' => applet.popMatrix()
      case 'F' =>
        val currentLen = turtleInfo.len * Math.pow(turtleInfo.scalingFactor, lSystem.generation).toFloat
        applet.line(0f, 0f, currentLen, 0f)
        applet.translate(currentLen, 0)
      case 'G' =>
        val currentLen = turtleInfo.len * Math.pow(turtleInfo.scalingFactor, lSystem.generation).toFloat
        applet.translate(currentLen, 0)
      case '+' => applet.rotate(turtleInfo.theta)
      case '-' => applet.rotate(-turtleInfo.theta)
    }
    )
    applet.popMatrix()
  }
}


object LSystemCollection {

  val tree = new Turtle(
    TurtleInfo(200, PApplet.radians(25), 300, 600, -PConstants.PI / 2),
    LSystem(
      "F",
      Map(
        'F' -> "FF+[+F-F-F]-[-F+F+F]"
      )
    ))

  val triangle = new Turtle(
    TurtleInfo(1200, PConstants.TWO_PI / 3, 600, 600, -PConstants.PI),
    LSystem(
      "F--F--F",
      Map(
        'F' -> "F--F--F--G",
        'G' -> "GG",
      )
    ))

  val squares = new Turtle(
    TurtleInfo(600, PConstants.PI / 2, 0, 600, 0, 1f/3),
    LSystem(
      "F-F-F-F",
      Map(
        'F' -> "F[F]-F+F[--F]+F-F",
      )
    ))
}

class LSystemsApp extends PApplet {

  var count = 0

  val turtle: Turtle = LSystemCollection.squares

  override def settings(): Unit = {
    size(600, 600, PConstants.P2D)
  }

  override def setup(): Unit = {
    colorMode(PConstants.HSB, 360)
    background(0)
    noFill()
    smooth()
    stroke(360, 60)
  }

  override def draw(): Unit = {
    background(0)
    turtle.render(this)
    noLoop()
  }

  override def mousePressed(): Unit = {
    turtle.redraw(this)
  }
}

object LSystems extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.lsystems.LSystemsApp")
  }
}