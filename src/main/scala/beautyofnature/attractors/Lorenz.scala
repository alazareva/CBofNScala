package beautyofnature.attractors

import processing.core.{PApplet, PConstants}


case class LorenzSystem(a: Float = 10, b: Float = 28, c: Float = 8f/3, dt: Float = 0.01f,
                       x: Float = 1f, y: Float = 1f, z: Float = 1f) {
  def next: LorenzSystem = {
    val dx = a * (y - x)
    val dy = x * (b - z) - y
    val dz = x * y - c * z
    LorenzSystem(a, b, c, dt, x + dx * dt, y + dy * dt, z + dz * dt)
  }
}

object LorenzSystem {
  def default: LorenzSystem = LorenzSystem()
}

class Lorenz extends PApplet {

  var system: LorenzSystem = LorenzSystem.default
  val points = 20000

  override def settings(): Unit = {
    size(600, 600, PConstants.P3D)
  }

  def drawSystem(): Unit = {
    system = LorenzSystem.default
    pushMatrix()
    translate(width / 2, height / 2)
    rotateX(frameCount * 0.005f)
    rotateY(frameCount * 0.005f)
    rotateZ(frameCount * 0.005f)
    scale(5)
    (0 to points).foreach(_ => {
      val nextSystem = system.next
      line(system.x, system.y, system.z, nextSystem.x, nextSystem.y, nextSystem.z)
      system = nextSystem
    })
    popMatrix()
  }

  override def setup(): Unit = {
    colorMode(PConstants.RGB, 255, 255, 255, 100)
    stroke(255)
    strokeWeight(0.1f)
    noFill()
  }

  override def draw(): Unit = {
    background(0)
    drawSystem()
  }
}

object Lorenz extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.attractors.Lorenz")
  }
}
