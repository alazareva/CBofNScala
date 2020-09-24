package beautyofnature.affinetransformations

import processing.core.{PApplet, PConstants}

import scala.util.Random



case class IFSTransformation(rules: List[TransformationRule], minX: Float, maxX: Float,
                             minY: Float, maxY: Float, iterations: Int = 100) {
  val determinants: List[Float] = rules.map(r => Math.max(Math.abs(r.a * r.d - r.b * r.c), 0.01f))
  val probas: List[Float] = determinants.map(_ / determinants.sum)


  def randomRule: TransformationRule  = {
    val r = Random.nextFloat
    def rec(curS: Float, curJ: Int): Int = {
      val nextS = curS + probas(curJ)
      if (nextS >= r) curJ
      else rec(nextS, curJ + 1)
    }
    rules(rec(0, 0))
  }

  def transform(p: Point): Point = p.transform(randomRule)
}

object IFSTransformations {

  val barnsleyFerm: IFSTransformation = IFSTransformation(List(
    TransformationRule( 0f,     0f,     0f,    0.16f, 0f, 0f),
    TransformationRule( 0.85f,  0.04f, -0.04f, 0.85f, 0f, 1.60f),
    TransformationRule( 0.20f, -0.26f,  0.23f, 0.22f, 0f, 1.60f),
    TransformationRule(-0.15f,  0.28f,  0.26f, 0.24f, 0f, 0.44f),
  ), -2.1820f, 2.6558f, 0f, 9.9983f, 1000)

  val mapleLeaf: IFSTransformation = IFSTransformation(List(
    TransformationRule( 0.1400f,  0.0100f,  0.0000f,  0.5100f, -0.0800f, -1.3100f),
    TransformationRule( 0.4300f,  0.5200f, -0.4500f,  0.5000f,  1.4900f, -0.7500f),
    TransformationRule( 0.4500f, -0.4900f,  0.4700f,  0.4700f, -1.6200f, -0.7400f),
    TransformationRule( 0.4900f,  0.0000f,  0.0000f,  0.5100f,  0.0200f,  1.6200f),
  ), -4f, 4f, -4f, 4f, 10000)
}


class IteratedFunctionalSystems extends PApplet {

  val transformation: IFSTransformation = IFSTransformations.mapleLeaf

  var point = Point(Random.nextFloat, Random.nextFloat)


  override def settings(): Unit = {
    size(600, 600, PConstants.P2D)
  }

  def drawPoint(p: Point): Unit = {
    val pt = mapPoint(p)
    point(pt.x, pt.y)
  }

  def mapPoint(p: Point): Point = {
    Point(
      PApplet.map(p.x, transformation.minX, transformation.maxX, 0,  width),
      PApplet.map(p.y, transformation.minY, transformation.maxY, height, 0),
    )
  }

  override def setup(): Unit = {
    colorMode(PConstants.RGB, 255, 255, 255, 100)
    background(255)
    noFill()
    smooth()
    stroke(0, 100, 0, 40)
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
    PApplet.main("beautyofnature.affinetransformations.IteratedFunctionalSystems")
  }
}