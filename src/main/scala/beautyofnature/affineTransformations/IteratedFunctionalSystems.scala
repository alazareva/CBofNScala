package beautyofnature.affineTransformations

import processing.core.{PApplet, PConstants}

import scala.util.Random



case class IFSTransformation(rules: List[TransformationRule], iterations: Int = 100) {
  val determinants = rules.map(r => Math.max(Math.abs(r.a * r.d - r.b * r.c), 0.01))
  val probas = determinants.map(_ / determinants.sum)


  def randomRule: TransformationRule  = {
    val r = Random.nextFloat
    def rec(curS: Float, curJ: Int): Int = {
      val nextS = curS + probas(curJ).toFloat
      if (nextS >= r) curJ
      else rec(nextS, curJ + 1)
    }
    rules(rec(0, 0))
  }

  def transform(p: Point): Point = p.transform(randomRule)
}

object IFSTransformations {
  val barnsleyFerm = IFSTransformation(List(
    TransformationRule( 0f,     0f,     0f,    0.16f, 0f, 0f),
    TransformationRule( 0.85f,  0.04f, -0.04f, 0.85f, 0f, 1.60f),
    TransformationRule( 0.20f, -0.26f,  0.23f, 0.22f, 0f, 1.60f),
    TransformationRule(-0.15f,  0.28f,  0.26f, 0.24f, 0f, 0.44f),
  ),
    1000
  )
}


class IteratedFunctionalSystems extends PApplet {

  val transformation: IFSTransformation = IFSTransformations.barnsleyFerm

  var point = Point(Random.nextFloat, Random.nextFloat)


  override def settings(): Unit = {
    size(600, 600, PConstants.P2D)
  }

  def drawPoint(p: Point) = {
    val pt = mapPoint(p)
    point(pt.x, pt.y)
  }

  def mapPoint(p: Point): Point = {
    Point(
      PApplet.map(p.x, -2.1820f, 2.6558f, 0,  width),
      PApplet.map(p.y, 0f, 9.9983f, height, 0),
    )
  }

  override def setup(): Unit = {
    colorMode(PConstants.RGB, 255, 255, 255, 100)
    background(255)
    noFill()
    smooth()
    stroke(0, 100, 0, 20)
    strokeWeight(1)
  }

  override def draw(): Unit ={
    if (frameCount >= transformation.iterations) {
      noLoop()
    }
    (0 to 500).foreach(_ => {
      drawPoint(point)
      point = transformation.transform(point)
    })
  }
}


object IteratedFunctionalSystems extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.affineTransformations.IteratedFunctionalSystems")
  }
}