package beautyofnature.affinetransformations

import processing.core.{PApplet, PConstants}

case class Rectangle(p1: Point, p2: Point, p3: Point, p4: Point) {
  def transform(rule: TransformationRule): Rectangle = {
    Rectangle(
      p1.transform(rule),
      p2.transform(rule),
      p3.transform(rule),
      p4.transform(rule)
    )
  }
}

case class Transformation(rules: List[TransformationRule], depth: Int = 5, bw: Float = 1.0f, bh: Float = 1.0f) {
  val initial = Rectangle(
    Point((1 + bw) / 2, (1 - bh) / 2),
    Point((1 - bw) / 2, (1 - bh) / 2),
    Point((1 - bw) / 2, (1 + bh) / 2),
    Point((1 + bw) / 2, (1 + bh) / 2),
  )

  def transformed: List[Rectangle] = {
    def rec(level: Int, acc: List[Rectangle]): List[Rectangle] = {
      if (level == 0) acc
      else {
       val current =  for {
          rule <- rules
          rect <- acc
        } yield rect.transform(rule)
        rec(level - 1, current)
      }
    }
    rec(depth, List(initial))
  }
}

object Transformations {
  val crystal = Transformation(
    List(
      TransformationRule(0f, -0.5f, 0.5f, 0f, 0.5f, 0f),
      TransformationRule(0f, 0.5f, -0.5f, 0f, 0.5f, 0.5f),
      TransformationRule(0.5f, 0f, 0f, 0.5f, 0.25f, 0.5f),
    ),
    10
  )
  val snowFlake = Transformation(
    List(
      TransformationRule(0.75f, 0f, 0f, 0.75f, 0.125f, 0.125f),
      TransformationRule(0.5f, -0.5f, 0.5f, 0.5f, 0.5f, 0f),
      TransformationRule(0.25f, 0f, 0f, 0.25f, 0f, 0.75f),
      TransformationRule(0.25f, 0f, 0f, 0.25f, 0.75f, 0.75f),
      TransformationRule(0.25f, 0f, 0f, 0.25f, 0f, 0f),
      TransformationRule(0.25f, 0f, 0f, 0.25f, 0.75f, 0f),
    ),
    1
  )

  val tree = Transformation(
    List(
      TransformationRule( 0.1950f, -0.4880f,  0.3440f,  0.4430f, 0.4431f, 0.2453f),
      TransformationRule( 0.4620f,  0.4140f, -0.2520f,  0.3610f, 0.2511f, 0.5692f),
      TransformationRule(-0.0580f, -0.0700f,  0.4530f, -0.1110f, 0.5976f, 0.0969f),
      TransformationRule(-0.0350f,  0.0700f, -0.4690f, -0.0220f, 0.4884f, 0.5069f),
      TransformationRule(-0.6370f,  0.0000f,  0.0000f,  0.5010f, 0.8562f, 0.2513f),
    ),
    7
  )
}

class AffineTransformations extends PApplet {

  val transformation: Transformation = Transformations.tree


  override def settings(): Unit = {
    size(600, 600, PConstants.P2D)
  }

  def drawRectangle(rect: Rectangle): Unit = {
    val r = mapRectangle(rect)
    line(r.p1.x, r.p1.y, r.p2.x, r.p2.y)
    line(r.p2.x, r.p2.y, r.p3.x, r.p3.y)
    line(r.p3.x, r.p3.y, r.p4.x, r.p4.y)
    line(r.p4.x, r.p4.y, r.p1.x, r.p1.y)
  }

  def mapPoint(p: Point): Point = {
    Point(
      PApplet.map(p.x, transformation.initial.p2.x, transformation.initial.p1.x, 10, width - 10),
      PApplet.map(p.y, transformation.initial.p1.y, transformation.initial.p4.y, height - 10, 10),
    )
  }

  def mapRectangle(rect: Rectangle): Rectangle = {
    Rectangle(
      mapPoint(rect.p1),
      mapPoint(rect.p2),
      mapPoint(rect.p3),
      mapPoint(rect.p4)
    )
  }

  override def setup(): Unit = {
    colorMode(PConstants.HSB, 360)
    background(360)
    noFill()
    smooth()
    stroke(0)
    transformation.transformed.foreach(drawRectangle)
    noLoop()
  }
}


object AffineTransformations extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.affinetransformations.AffineTransformations")
  }
}