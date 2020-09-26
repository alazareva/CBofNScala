package beautyofnature.evolution

import processing.core.{PApplet, PConstants}

import scala.util.Random

case class AssignmentIndividual(assignments: List[Int]) extends Individual

class AssignmentPopulation(
  val populationSize: Int,
  crossoverRate: Float,
  mutationRate: Float,
  costs: List[List[Double]]
) extends Population[AssignmentIndividual] {

  override var individuals: Array[AssignmentIndividual] = Array.fill(populationSize)(randomIndividual)

  override def individualFitness(individual: AssignmentIndividual): Double =
    individual.assignments.indices.map(i => costs(i)(individual.assignments(i))).sum

  override def randomIndividual: AssignmentIndividual = AssignmentIndividual(Random.shuffle(costs.indices.toList))

  override def fitness: Array[Double] = {
    val rawFitness = individuals.map(individualFitness)
    val min = rawFitness.min
    val normalized = rawFitness.map(f => Math.pow(2, f - min))
    val total = normalized.sum
    if (total == 0) normalized else normalized.map(_ / total)

  }

  def mutateArray(arr: Array[Int]): Array[Int] = {
    arr.indices.foreach { i =>
      if (Random.nextFloat < mutationRate) {
        val j = Random.nextInt(arr.length)
        val a = arr(i)
        arr(i) = arr(j)
        arr(j) = a
      }
    }
    arr
  }

  override def reproduce(
    individual1: AssignmentIndividual,
    individual2: AssignmentIndividual
  ): Array[AssignmentIndividual] = {
    val arr1 = individual1.assignments.toArray
    val arr2 = individual2.assignments.toArray
    if (Random.nextFloat < crossoverRate) {
      val ai = Random.nextInt(individual1.assignments.length)
      val a = arr1(ai)
      val b = arr2(ai)
      val bi = arr1.indexWhere(_ == b)
      arr1(ai) = b
      arr1(bi) = a

      val c = arr2(bi)
      arr2(ai) = c
      arr2(bi) = b
    }
    Array(
      AssignmentIndividual(mutateArray(arr1).toList),
      AssignmentIndividual(mutateArray(arr2).toList)
    )
  }

}

object AssignmentPopulation {
  def apply(populationSize: Int, crossoverRate: Float, mutationRate: Float, costs: List[List[Double]]) =
    new AssignmentPopulation(populationSize, crossoverRate, mutationRate, costs)
}

object AssignmentData {

  val default = List(
    List(10.0, 5.0, 4.0, 6.0, 5.0, 1.0),
    List(6.0, 4.0, 9.0, 7.0, 3.0, 2.0),
    List(1.0, 8.0, 3.0, 6.0, 4.0, 6.0),
    List(5.0, 3.0, 7.0, 2.0, 1.0, 4.0),
    List(3.0, 2.0, 5.0, 6.0, 8.0, 7.0),
    List(7.0, 6.0, 4.0, 1.0, 3.0, 2.0)
  )
}


class GAAssignment extends PApplet {

  val costs = AssignmentData.default
  val populationSize = 10
  val crossoverRate = 0.75f
  val mutationRate = 0.01f

  val population = AssignmentPopulation(populationSize, crossoverRate, mutationRate, costs)

  val w = 600
  val h = 600

  val scale = 0.5

  val dataW = costs.length
  val dataH = costs(0).length
  val rectW = w / dataW
  val rectH = h / dataH

  override def settings(): Unit = {
    size(w, h + 100, PConstants.P2D)
  }

  override def setup(): Unit = {
    background(255)
    fill(0)
    textMode(PConstants.CENTER)
  }

  def showGrid(assignment: AssignmentIndividual): Unit = {
    textSize(40)
    for {
      i <- 0 until dataW
      j <- 0 until dataH
    } {
      val rectFill = if (assignment.assignments(i) == j) 0 else 255
      fill(rectFill)
      rect(i * rectW, j * rectH, rectW, rectH)
      fill(255 - rectFill)
      text(f"${costs(j)(i).toInt}", i * rectW + 15, j * rectH + rectH / 2)
    }
  }

  override def draw(): Unit = {
    population.next()
    if (frameCount % 2 == 0) {
      background(255)
      val top = population.population.sortBy(i => population.individualFitness(i)).reverse.head
      showGrid(top)
      textSize(50)
      text(f"${population.individualFitness(top)}", w / 2 - 50, h + 50)
    }
  }
}

object GAAssignment extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.evolution.GAAssignment")
  }
}

