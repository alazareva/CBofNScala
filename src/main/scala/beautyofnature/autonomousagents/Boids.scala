package beautyofnature.autonomousagents

import processing.core.{PApplet, PConstants, PVector}

import scala.util.Random


case class Boid(position: PVector, velocity: PVector, newVelocity: PVector)

class Boids extends PApplet {

  val w = 640
  val h = 480
  val numBoids = 40
  val boidSize = 3
  val angle = 270f
  val visualAngle = 90f
  val momentumFactor = 0.95f
  val timeStep = 1.0f
  val radiusCopy = 60 //80
  val radiusCenter = 30
  val radiusVisualAvoidance = 40
  val radiusAvoidance = 20
  val weightCopy = 0.2f
  val weightCenter = 0.4f
  val weightVisualAvoidance = 0.8f
  val weightAvoidance = 1.0f
  val maxVelocity = 1.5f

  val maxRadius = List(radiusVisualAvoidance, radiusCopy, radiusCenter, radiusAvoidance).max
  val cosAngle = Math.cos(Math.toRadians(angle / 2))
  val cosVisualAngle = Math.cos(Math.toRadians(visualAngle / 2))

  def randomBoid: Boid = Boid(
    new PVector(Random.nextInt(w - 50), Random.nextInt(h - 50)),
    new PVector(-1 + Random.nextInt(3), -1 + Random.nextInt(3)).normalize(),
    new PVector(0, 0)
  )

  val boids: Array[Boid] = Array.fill(numBoids)(randomBoid)

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
          if (distance <= radiusCenter && distance > radiusAvoidance) {
            a.add(vector)
            numCentered += 1
          }
          if (distance <= radiusCopy && distance > radiusAvoidance) b.add(otherBoid.velocity)
          if (distance <= radiusAvoidance) c.add(PVector.sub(firstBoid.position, otherBoidPosition).normalize())
          if (distance <= radiusVisualAvoidance && cosVisualAngle < cos) {
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
    val vt = a.mult(weightCenter)
      .add(b.mult(weightCopy))
      .add(c.mult(weightAvoidance))
      .add(d.mult(weightVisualAvoidance))
    val newVelocity = firstBoid.velocity.mult(momentumFactor).add(vt.mult(1 - momentumFactor))
    firstBoid.newVelocity.set(newVelocity.setMag(maxVelocity))
  }

  def update(): Unit = {
    boids.indices.foreach(computeBoidHeading)
    boids.foreach { boid =>
      boid.velocity.set(boid.newVelocity)
      boid.position.add(boid.velocity.mult(timeStep))
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
    vertex(0, -boidSize * 2)
    vertex(-boidSize, boidSize * 2)
    vertex(boidSize, boidSize * 2)
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


