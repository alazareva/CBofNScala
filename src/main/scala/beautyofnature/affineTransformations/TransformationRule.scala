package beautyofnature.affineTransformations

case class TransformationRule(a: Float, b: Float, c: Float, d: Float, e: Float, f: Float)

case class Point(x: Float, y: Float) {
  def transform(rule: TransformationRule): Point = Point(
    rule.a * x + rule.b * y + rule.e,
    rule.c * x + rule.d * y + rule.f
  )
}
