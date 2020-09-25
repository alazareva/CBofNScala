package beautyofnature.analogcomputation

import processing.core.{PApplet, PConstants}

import scala.util.Random



case class Neuron(state: Double, nextState: Double, activation: Double, inputs: Double, cost: Double)


object HopfieldData {

  val default = Array(
    Array(10.0,	5.0,	4.0,	6.0,	5.0,	1.0),
    Array(6.0,	4.0,	9.0,	7.0,	3.0,	2.0),
    Array(1.0,	8.0,	3.0,	6.0,	4.0,	6.0),
    Array(5.0,	3.0,	7.0,	2.0,	1.0,	4.0),
    Array(3.0,	2.0,	5.0,	6.0,	8.0,	7.0),
    Array(7.0,	6.0,	4.0,	1.0,	3.0,	2.0)
  )
}


class Hopfield extends PApplet{

  val dt = 0.1
  val tau = 10.0

  val w = 300
  val h = 300

  val scale = 0.5

  val data: Array[Array[Double]] = HopfieldData.default
  val dataW = data.length
  val dataH = data(0).length
  val rectW = w / dataW
  val rectH = h / dataH

  val network = initializeNetwork(data)

  override def settings(): Unit = {
    size(w, h, PConstants.P2D)
  }

  override def setup(): Unit = {
    frameRate = 1
    strokeWeight(0.5f)
    stroke(100)
  }

  def sigmoid(x: Double): Double = Math.exp(x) / (Math.exp(x) + 1)

  def initializeNetwork(data: Array[Array[Double]]): Array[Array[Neuron]] = {
    val neurons = data.map(_.map(d => {
      val randomState = - 1 + Random.nextDouble() * 2
      Neuron(randomState, 0.0, sigmoid(randomState), d, d)
    }))
    val activations = neurons.flatMap(_.map(_.inputs))
    val min = activations.min
    val max = activations.max
    val ave = activations.sum / activations.length

    neurons.map(_.map(n => n.copy(inputs = scale * (n.inputs - ave) / (max - min) + 2)))
  }

  def showNetwork(): Unit = {
    for {
      i <- 0 until  dataW
      j <- 0 until dataH
    } {
      fill(255 - network(j)(i).activation.toFloat * 255)
      rect(i * rectW, j * rectH, rectW, rectH)
    }
  }

  def updateActivations(): Unit = {
    for {
      i <- 0 until  dataW
      j <- 0 until dataH
    } {
      network(j)(i) = {
        val n = network(j)(i)
        n.copy(activation = sigmoid(n.state))
      }
    }
  }

  def updateStates(): Unit = {
    for {
      i <- 0 until  dataW
      j <- 0 until dataH
    } {
      val rowSum = (0 until dataW).foldLeft(0.0){case (s, ni) =>
        if (ni != i) s + -2 * network(j)(ni).activation else s
      }

      val colSum = (0 until dataH).foldLeft(0.0){case (s, nj) =>
        if (nj != j) s + -2 * network(nj)(i).activation else s
      }

      val sum = rowSum + colSum

      val n = network(j)(i)
      network(j)(i) = n.copy(nextState = n.state + dt * (sum + n.inputs - n.state / tau))
    }
  }

  def resetStates(): Unit = {
    for {
      i <- 0 until  dataW
      j <- 0 until dataH
    } {
      val n = network(j)(i)
      network(j)(i) = n.copy(state = n.nextState, nextState = n.state)
    }
  }

  def update(): Unit = {
    updateActivations()
    updateStates()
    resetStates()
  }

  override def draw(): Unit = {
    showNetwork()
    update()
  }
}

object Hopfield extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.analogcomputation.Hopfield")
  }
}
