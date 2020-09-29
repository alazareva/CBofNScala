package beautyofnature.autonomousagents

import processing.core.{PApplet, PConstants, PVector}

import scala.util.Random


case class Boid(position: PVector, velocity: PVector, newVelocity: PVector)

case class BoidsConfig(
  numBoids: Int,
  boidSize: Int,
  angle: Float,
  visualAngle: Float,
  momentumFactor: Float,
  timeStep: Float,
  radiusCopy: Int,
  radiusCenter: Int,
  radiusVisualAvoidance: Int,
  radiusAvoidance: Int,
  weightCopy: Float,
  weightCenter: Float,
  weightVisualAvoidance: Float,
  weightAvoidance: Float,
  maxVelocity: Float,
)

object BoidConfigs {

  val default = BoidsConfig(
    40,
    3,
    270f,
    90f,
    0.95f,
    1.0f,
    60,
    30,
    40,
    20,
    0.2f,
    0.4f,
    0.8f,
    1.0f,
    1.5f
  )
}


class Boids extends PApplet {

  val w = 640
  val h = 480

  val config = BoidConfigs.default
  val maxRadius = List(config.radiusVisualAvoidance, config.radiusCopy, config.radiusCenter, config.radiusAvoidance).max
  val cosAngle = Math.cos(Math.toRadians(config.angle / 2))
  val cosVisualAngle = Math.cos(Math.toRadians(config.visualAngle / 2))

  def randomBoid: Boid = Boid(
    new PVector(Random.nextInt(w - 50), Random.nextInt(h - 50)),
    new PVector(-1 + Random.nextInt(3), -1 + Random.nextInt(3)).normalize(),
    new PVector(0, 0)
  )

  val boids: Array[Boid] = Array.fill(config.numBoids)(randomBoid)

  override def settings(): Unit = {
    size(w, h, PConstants.P2D)
  }

  def orthogonal(vec: PVector, inDirectionOf: PVector): PVector = {
    val candidate = if (vec.x != 0 && vec.y != 0) {
      val ratio = Math.pow(vec.y / vec.x, 2)
      val u = Math.sqrt(ratio / (1 + ratio))
      val v = -vec.x * u / vec.y
      new PVector(u.toFloat, v.toFloat)
    } else if (vec.x != 0) {
      new PVector(1, 0)
    } else if (vec.y != 0) {
      new PVector(0, 1)
    } else new PVector(0, 0)
    if (PVector.dot(candidate, inDirectionOf) < 0) candidate.rotate(PConstants.PI) else candidate
  }

  def getOtherBoidPosition(thisBoid: Boid, otherBoid: Boid): PVector = {
    val other = otherBoid.position
    val current = thisBoid.position
    val candidates = for {
      j <- List(-w, 0, w)
      k <- List(-h, 0, h)
    } yield PVector.add(other, new PVector(j, k))
    candidates.foldLeft((other, PVector.dist(other, current))) { case (best, candidate) =>
      val (bestV, minDist) = best
      val d = PVector.dist(candidate, current)
      if (d < minDist) (candidate, d) else best
    }._1
  }


  def computeBoidHeading(i: Int): Unit = {
    var numCentered = 0
    val a = new PVector(0, 0)
    val b = new PVector(0, 0)
    val c = new PVector(0, 0)
    val d = new PVector(0, 0)
    val firstBoid = boids(i)
    for {
      j <- boids.indices
      if i != j
    } {
      val otherBoid = boids(j)
      val otherBoidPosition = getOtherBoidPosition(firstBoid, otherBoid)
      val distance = PVector.dist(firstBoid.position, otherBoidPosition)
      if (distance <= maxRadius) {
        val vector = PVector.sub(otherBoidPosition, firstBoid.position)
        val cos = Math.cos(PVector.angleBetween(firstBoid.velocity, vector))
        if (cos >= cosAngle) {
          if (distance <= config.radiusCenter && distance > config.radiusAvoidance) {
            a.add(vector)
            numCentered += 1
          }
          if (distance <= config.radiusCopy && distance > config.radiusAvoidance) b.add(otherBoid.velocity)
          if (distance <= config.radiusAvoidance) c.add(PVector.sub(firstBoid.position, otherBoidPosition).normalize())
          if (distance <= config.radiusVisualAvoidance && cosVisualAngle < cos) {
            val dist = PVector.sub(firstBoid.position, otherBoidPosition)
            val divBy = if (dist.mag() == 0) 1 else dist.mag()
            d.add(PVector.add(dist, orthogonal(dist, firstBoid.velocity)).div(divBy))
          }
        }
      }
    }
    if (numCentered < 2) a.set(0, 0)
    a.normalize()
    b.normalize()
    c.normalize()
    d.normalize()
    val vt = a.mult(config.weightCenter)
      .add(b.mult(config.weightCopy))
      .add(c.mult(config.weightAvoidance))
      .add(d.mult(config.weightVisualAvoidance))
    val newVelocity = firstBoid.velocity.mult(config.momentumFactor).add(vt.mult(1 -config. momentumFactor))
    firstBoid.newVelocity.set(newVelocity.setMag(config.maxVelocity))
  }

  def update(): Unit = {
    boids.indices.foreach(computeBoidHeading)
    boids.foreach { boid =>
      boid.velocity.set(boid.newVelocity)
      boid.position.add(boid.velocity.mult(config.timeStep))
      if (boid.position.x < 0) boid.position.x += width
      else if (boid.position.x >= w) boid.position.x -= width
      if (boid.position.y < 0) boid.position.y += h
      else if (boid.position.y >= h) boid.position.y -= h
    }
  }

  def drawBoid(boid: Boid): Unit = {
    fill(0)
    pushMatrix()
    translate(boid.position.x, boid.position.y)
    rotate(boid.velocity.heading() + PConstants.HALF_PI)
    beginShape(PConstants.TRIANGLES)
    vertex(0, - config.boidSize * 2)
    vertex(-config.boidSize, config.boidSize * 2)
    vertex(config.boidSize, config.boidSize * 2)
    endShape()
    popMatrix()
  }

  override def setup(): Unit = {
    frameRate = 1
  }

  override def draw(): Unit = {
    background(255)
    boids.foreach(drawBoid)
    update()
  }
}

object Boids extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.autonomousagents.Boids")
  }
}


