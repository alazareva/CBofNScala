package beautyofnature.autonomousagents

import processing.core.{PApplet, PConstants, PVector}

import scala.util.Random


case class Boid(position: PVector, velocity: PVector, newVelocity: PVector) {

}

class Boids extends PApplet {

  val w = 640
  val h = 480
  val num = 20
  val len = 3
  val angle = 270f
  val vangle = 90f
  val minv = 0.5f
  val ddt  = 0.95f
  val dt = 3.0f
  val rcopy = 80
  val rcent = 30
  val rviso = 40
  val rvoid = 15
  val wcopy = 0.2f
  val wcent = 0.4f
  val wviso = 0.8f
  val wvoid = 1.0f
  val wrand = 0.0f

  val maxr = List(rviso, rcopy, rcent, rvoid).max
  val cosangle = Math.cos(angle / 2)
  val cosvangle = Math.cos(vangle / 2)

  def randomBoid: Boid = Boid(
    new PVector(Random.nextInt(w - 50), Random.nextInt(h - 50)),
    new PVector( -1 + Random.nextInt(3), -1 + Random.nextInt(3)).normalize(),
    new PVector(0, 0)
    )

  val boids: Array[Boid] = Array.fill(num)(randomBoid)

  override def settings(): Unit = {
    size(w, h, PConstants.P2D)
  }

  def orthogonal(vec: PVector, inDirectionOf: PVector): PVector = {
    val candidate = if (vec.x != 0 && vec.y != 0) {
      val ratio = Math.pow(vec.y / vec.x, 2)
      val u = Math.sqrt(ratio / (1 + ratio))
      val v = - vec.x * u / vec.y
      new PVector(u.toFloat, v.toFloat)
    } else if (vec.x == 0) {
      new PVector(1, vec.y)
    } else if (vec.y == 0) {
      new PVector(vec.x, 1)
    } else new PVector(0, 0)
    if (PVector.dot(candidate, inDirectionOf) < 0) candidate.rotate(PConstants.HALF_PI) else candidate
  }

  def computeBoidHeading(i: Int): Unit = {
    var numCentered = 0
    val a = new PVector(0, 0)
    val b = new PVector(0, 0)
    val c = new PVector(0, 0)
    val d = new PVector(0, 0)
    val firstBoid = boids(i)
    print(f"Velocity Before update ${firstBoid.velocity}")
    for {
      j <- boids.indices
      if i != j
    } {
      val secondBoid = boids(j)
      val distance = PVector.dist(firstBoid.position, secondBoid.position)
      if (distance <= maxr) {
        val vector = PVector.sub(firstBoid.position, secondBoid.position)
        val cos = Math.cos(PVector.angleBetween(firstBoid.velocity, vector))
        if (cos >= cosangle) {

          if(distance <= rcent && distance > rvoid) {
            a.add(PVector.sub(secondBoid.position, firstBoid.position))
            numCentered += 1
          }
          if (distance <= rcopy && distance > rvoid) b.add(secondBoid.velocity)
          if (distance <= rvoid) c.add(PVector.sub(firstBoid.position, secondBoid.position).normalize())
          if (distance <= rviso && cosvangle < cos) {
            val temp = PVector.sub(firstBoid.position, vector)
            val orth = orthogonal(temp, firstBoid.velocity)
            val divBy = if (distance == 0) 1 else distance
            val diff  = PVector.add(vector, orth).div(divBy)
            d.add(diff)
          }
        }
      }
    }
    if (numCentered < 0) a.set(0, 0)
    a.normalize()
    b.normalize()
    c.normalize()
    d.normalize()
    val vt = a.mult(wcent).add(b.mult(wcopy)).add(c.mult(wvoid)).add(d.mult(wviso))
    val newVelocity = firstBoid.velocity.mult(ddt).add(vt.mult(1 - ddt))
    val dd = newVelocity.mag()
    if (dd < minv) newVelocity.mult(minv / dd)
    firstBoid.newVelocity.set(newVelocity)
    println(f"Velocity Before update ${firstBoid.newVelocity}")
  }

  def update(): Unit = {
    boids.indices.foreach(computeBoidHeading)
    boids.foreach{ boid =>
      boid.velocity.set(boid.newVelocity)
      boid.position.add(boid.velocity.mult(dt))
      if (boid.position.x < 0) boid.position.x += width
      else if (boid.position.x >= w) boid.position.x -= width
      if (boid.position.y < 0) boid.position.y += h
      else if (boid.position.y >= h) boid.position.y -= h
    }
  }

  def drawBoid(boid: Boid): Unit = {
    //println(boid.position.x)
    fill(0)
    pushMatrix()
    translate(boid.position.x, boid.position.y)
    rotate(boid.velocity.heading())
    triangle(-5, 0, 0, 20, 5, 0)
    popMatrix()
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


