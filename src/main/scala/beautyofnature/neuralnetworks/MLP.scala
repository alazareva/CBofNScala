package beautyofnature.neuralnetworks

import processing.core.{PApplet, PConstants}

import scala.util.Random


case class NeuralNetworkConfig(
  numInputs: Int,
  numHidden: Int,
  numOutputs: Int,
  learningRate: Float,
  momentumRate: Float,
  weightInit: Float,
)

case class Layer(w: Weights, b: Bias)
case class Weights(w: Array[Array[Double]], g: Array[Array[Double]], d: Array[Array[Double]])
case class Bias(w: Array[Double], g: Array[Double], d: Array[Double])


// y = sigmoid(Vz + b) = sigmoid(V sigmoid(Ux + a) + b)
class NN(
  val V: Weights,
  val U: Weights,
  val a: Bias,
  val b: Bias,
  val learningRate: Float,
  val momentumRate: Float,
  val numInputs: Int,
  val numHidden: Int,
  val numOutputs: Int
) {

  def sigmoid(x: Double): Double = 1 / (1 + Math.exp(-x))

  def getZ(x: List[Double]): Array[Double] = {
    // z = sigmoid(Ux + a)
    val z = a.w.clone
    for {
      i <- z.indices
      j <- x.indices
    } {
      z(i) += x(j) * U.w(i)(j)
    }
    z.map(sigmoid)
  }

  def getY(z: Array[Double]): Array[Double] = {
    // y = sigmoid(Vz + b)
    val y = b.w.clone()
    for {
      i <- y.indices
      j <- z.indices
    } {
      y(i) = y(i) + z(j) * V.w(i)(j)
    }
    y.map(sigmoid)
  }

  def error(y: Array[Double], target: List[Double]): Double = {
    y.zip(target).map { case (d1, d2) => Math.pow(d1 - d2, 2) }.sum / y.length
  }

  def feedforward(x: List[Double], target: List[Double]): (Double, Array[Double], Array[Double]) = {
    val z = getZ(x)
    val y = getY(z)
    (error(y, target), z, y)
  }

  def feedback(x: List[Double], target: List[Double], y: Array[Double], z: Array[Double]): Double = {
    // dE/dy[i] = y[i] - target[i]
    val gy = y.zip(target).map { case (d1, d2) => d1 - d2 }
    val error = gy.map(Math.pow(_, 2)).sum

    // dE/dy_in[i] = dE/dy[i] * y[i] * (1 - y[i])
    // dE/db[i] = dE/dy_in[i]
    gy.indices.foreach { i =>
      gy(i) *= y(i) * (1 - y(i))
      b.g(i) = gy(i)
    }

    z.indices.foreach { i =>
      val gz = Array.fill(z.length)(0.0)
      gy.indices.foreach { j =>
        // dE/dv[i,j] = dE/dy[j] * z[i]
        V.g(j)(i) = gy(j) * z(i)
        // dE/dz[i] = sum_j ( dE/dy_in[j] * v[j][i] )
        gz(i) =  gz(i) + gy(j) * V.w(j)(i)
      }
      // dE/dz_in[i] = dE/dz[i] * z[i] * (1 - z[i])
      // dE/da[i] = dz_in[i]
      gz.indices.foreach { i =>
        gz(i) = gz(i) * (z(i) * (1 - z(i)))
        a.g(i) = gz(i)
      }

      // dE/du[i][j] = dz_in[i] * x[j]
      x.indices.foreach { j =>
        U.g(i)(j) = gz(i) * x(j)
      }
    }
    error / target.length
  }

  def updateWeights(w: Weights): Unit = {
    w.w.indices.foreach {i =>
      w.w(i).indices.foreach {j =>
        w.d(i)(j) = momentumRate * w.d(i)(j) - learningRate * w.g(i)(j)
        w.w(i)(j) += w.d(i)(j)
      }
    }
  }

  def updateBias(b: Bias): Unit = {
    b.w.indices.foreach { i =>
      b.d(i) = momentumRate * b.d(i) - learningRate * b.g(i)
      b.w(i) += b.d(i)
    }
  }

  // update the based on current gradients and previous weight delta using momentum
  def update(): Unit = {
    // Delta_VAR(t + 1) = momentum * Delta_VAR(t) - rate * Grad_VAR(t + 1)
    // VAR(t + 1) = VAR(t) + Delta_VAR(t + 1)
    updateBias(a)
    updateWeights(U)
    updateBias(b)
    updateWeights(V)
  }

  def trainingStep(x: List[Double], target: List[Double]): Unit = {
    val (_, z, y) = feedforward(x, target)
    feedback(x, target, y, z)
    update()
  }
}

object NN {

def apply(config: NeuralNetworkConfig): NN = {
  def randomWeight: Double = (-1 + Random.nextDouble * 2) * config.weightInit
  val a = Bias(
    Array.fill(config.numHidden)(randomWeight),
    Array.fill(config.numHidden)(0),
    Array.fill(config.numHidden)(0)
  )
  val b = Bias(
    Array.fill(config.numOutputs)(randomWeight),
    Array.fill(config.numOutputs)(0),
    Array.fill(config.numOutputs)(0)
  )
  val U = Weights(
    Array.fill(config.numHidden, config.numInputs)(randomWeight),
    Array.fill(config.numHidden, config.numInputs)(0),
    Array.fill(config.numHidden, config.numInputs)(0)
  )
  val V = Weights(
    Array.fill(config.numOutputs, config.numHidden)(randomWeight),
    Array.fill(config.numOutputs, config.numHidden)(0),
    Array.fill(config.numOutputs, config.numHidden)(0)
  )
  new NN(V, U, a, b, config.learningRate, config.momentumRate, config.numInputs, config.numHidden, config.numOutputs)
}
}


object NNConfigs {

  val default = NeuralNetworkConfig(
    numInputs = 2,
    numHidden = 2,
    numOutputs = 1,
    learningRate = 0.25f,
    momentumRate = 0.5f,
    weightInit = 0.1f,
  )

}

object XOR {
  """0	0	0
    |0	1	1
    |1	0	1
    |1	1	0"""
  val x: List[List[Double]] = List(
    List(0, 0),
    List(0, 1),
    List(1, 0),
    List(1, 1),
  )
  val target: List[List[Double]] = List(
    List(0),
    List(1),
    List(1),
    List(2)
  )
}

class MLP extends PApplet {

  val config = NNConfigs.default
  val network = NN(config)

  val leftPadding = 100
  val topPadding = 100
  val horizontalSpacing = 200
  val verticalSpacing = 200

  override def settings(): Unit = {
    size(800, 600, PConstants.P2D)
  }

  override def setup(): Unit = {
    smooth()
  }

  def drawU(): Unit = {
    pushStyle()
    for {
      i <- 0 until config.numInputs
      j <- 0 until config.numHidden
    } {
      val x1 = leftPadding
      val y1 = topPadding + verticalSpacing * i
      val x2 = leftPadding + horizontalSpacing
      val y2 = topPadding + verticalSpacing * j
      stroke(0)
      line(x1, y1, x2, y2)
      val textOffset = 45
      val offsetY = if (y1 == y2) 0 else (y2 - y1) / (x2 - x1) * textOffset
      fill(255)
      noStroke()
      circle(x1 + textOffset * 1.5f, y1 + offsetY, 50)
      fill(0)
      textSize(14)
      text(f"${network.U.w(j)(i)}%1.2f", leftPadding + textOffset, topPadding + verticalSpacing * i + offsetY)
    }
    popStyle()
  }

  def drawV(): Unit = {
    (0 until config.numHidden).foreach{i => {
      val x1 = leftPadding + horizontalSpacing
      val y1 = topPadding + verticalSpacing * i
      val x2 = leftPadding + 2 * horizontalSpacing
      val y2 = topPadding + 0.5f * verticalSpacing
      line(x1, y1, x2, y2)
      val textOffset = 45
      val offsetY = if (y1 == y2) 0 else (y2 - y1) / (x2 - x1) * textOffset
      fill(255)
      noStroke()
      circle(x1 + textOffset * 1.5f, y1 + offsetY, 50)
      fill(0)
      textSize(14)
      text(f"${network.V.w(0)(i)}%1.2f", leftPadding + textOffset, topPadding + verticalSpacing * i + offsetY)
    }}
  }

  def drawNetwork(): Unit = {
    drawU()
    drawV()
    (0 until config.numInputs).foreach(i => circle(leftPadding, topPadding + verticalSpacing * i, 10))
    (0 until config.numHidden).foreach(i => circle(leftPadding + horizontalSpacing, topPadding + verticalSpacing * i, 10))
    circle(leftPadding + 2 * horizontalSpacing, topPadding + 0.5f * verticalSpacing, 10)
  }

  override def draw(): Unit = {
    background(255)
    drawNetwork()
  }
}

object MLP extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.neuralnetworks.MLP")
  }
}

