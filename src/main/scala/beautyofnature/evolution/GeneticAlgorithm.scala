package beautyofnature.evolution

import scala.reflect.ClassTag
import scala.util.Random


trait Individual

trait Population[T <: Individual] {

  def populationSize: Int

  var individuals: Array[T]

  def randomIndividual: T

  def individualFitness(individual: T): Double

  def fitness: Array[Double]

  def selectOne(populationFitness: Array[Double]): T = {
    val p = Random.nextDouble

    def rec(i: Int, sum: Double): T = {
      if (i >= individuals.length) individuals(individuals.length - 1)
      else if (p <= sum + populationFitness(i)) individuals(i)
      else rec(i + 1, sum + populationFitness(i))
    }

    rec(0, 0.0)
  }

  def reproduceTwo(individual1: T, individual2: T): Array[T]

  def reproduce(populationFitness: Array[Double]): Array[T] = {
    val individual1 = selectOne(populationFitness)
    val individual2 = selectOne(populationFitness)
    reproduceTwo(individual1, individual2)
  }

  def next()(implicit c: ClassTag[T]): Unit = {
    individuals = (0 until individuals.length / 2).flatMap(_ => reproduce(fitness)).toArray
  }

  def population: List[T] = individuals.toList
}
