package beautyofnature.cellularautomata

import processing.core.{PApplet, PConstants}


case class AutomataConfig(rules: String, init: String, width: Int, wrap: Boolean = true)

object Automata {
  def apply(config: AutomataConfig): Automata = {
    val states = config.rules.distinct.length
    val radius = (((config.rules.length - 1) / (states - 1)) - 1) / 2

    val arr = Array.fill(config.width + 2 * radius + 2)(0)

    config.init.zipWithIndex.foreach { case (c, j) =>
      val i = (config.width - config.init.length) / 2 + radius + 1 + j
      arr(i) = c - '0'
    }
    new Automata(arr, states, radius, config)
  }
}

class Automata(private var arr: Array[Int], val states: Int, radius: Int, val config: AutomataConfig) {

  def generate(i: Int): Array[Int] = {
    val newA = Array.fill(config.width + 2 * radius + 2)(0)
    if (config.wrap) {
        (0 until radius).foreach(j => {
          arr(j + 1) = arr(config.width + 1 + j)
          arr(config.width + radius + 1 + j) = arr(radius + 1 + j)
        })
      }
      var sum = arr.take(radius).sum
      (radius + 1 until config.width + radius + 1).foreach( j => {
        sum = sum + arr(j + radius) - arr(j - radius - 1)
        if (sum >= 0 && sum < config.rules.length) newA(j) = config.rules(sum) - '0'
      })
    arr = newA
    arr
  }

  def lambda: Double = {
    val area = 2 * radius + 1
    val len = (states - 1) * area + 1
    val N = Math.pow(states, area)
    val table = Array.ofDim[Int](area, len)

    // initial condition
    (0 until len).foreach(i => table(0)(i) = if (i < states) 1 else 0)

    for {
      i <- 1 until area
      j <- 0 until len
    } {
      (0 until states).foldLeft(0){ (acc, k) =>
        val sum = if (j - k >= 0) acc + table(i - 1)(j - k) else acc
        table(i)(j) = sum
        sum
      }
    }
    val vals = table(area - 1).map(_ / N)
    config.rules.zip(vals).foldLeft(0.0){ case (lam, (c, v)) => if (c != '0') lam + v else lam}
  }
}


object AutomataConfigs {
  val triangle = AutomataConfig(
    "01010", //"01010",
    "101", //"11",
    640
  )

  val triangle2 = AutomataConfig(
    "2012122222",
    "1220202020200000222221021002001012",
    640
  )
}

class CellularAutomata extends PApplet {

  val automataConfig: AutomataConfig = AutomataConfigs.triangle2
  val automata: Automata = Automata(automataConfig)

  override def settings(): Unit = {
    size(automataConfig.width, 400, PConstants.P2D)
  }

  override def setup(): Unit = {
    background(255)
    strokeWeight(1)
    println(automata.lambda)
  }

  override def draw(): Unit = {
    val j = frameCount
    val arr = automata.generate(j)
    arr.zipWithIndex.foreach { case (v, i) =>
      stroke(PApplet.map(v, 0, automata.states - 1, 0, 255))
      point(i, j)
    }
    if (j == height) noLoop()
  }
}

object CellularAutomata extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.cellularautomata.CellularAutomata")
  }
}

