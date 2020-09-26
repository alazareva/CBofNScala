package beautyofnature.evolution


import processing.core.{PApplet, PConstants}

import scala.util.Random


case class Individual(s: String) {
  def fitness(target: String): Double = {
    val fit = s.zip(target).foldLeft(0) { case (f, (c1, c2)) => if (c1 == c2) f + 1 else f }
    Math.pow(2, fit - target.length)
  }
}


class Population(populationSize: Int, crossoverRate: Float, mutationRate: Float, target: String) {

  var individuals: Array[Individual] = Array.fill(populationSize)(randomIndividual)

  def randomIndividual: Individual = Individual((0 until target.length).map(_ => randomLetterOrSpace).mkString)

  def fitness: Array[Double] = {
    val rawFitness = individuals.map(_.fitness(target))
    val total = rawFitness.sum
    if (total == 0) rawFitness else rawFitness.map(_ / total)
  }

  def selectOne(populationFitness: Array[Double]): Individual = {
    val p = Random.nextDouble

    def rec(i: Int, sum: Double): Individual = {
      if (i >= individuals.length) individuals(individuals.length - 1)
      else if (p <= sum + populationFitness(i)) individuals(i)
      else rec(i + 1, sum + populationFitness(i))
    }

    rec(0, 0.0)
  }

  def randomLetterOrSpace: Char = {
    val low = 'a'.toInt
    val high = 'z'.toInt + 2
    val ord = Random.nextInt(high - low) + low
    if (ord > 'z'.toInt) ' ' else ord.toChar
  }

  def mutate(c: Char): Char = if (Random.nextFloat < mutationRate) randomLetterOrSpace else c

  def reproduce(individual1: Individual, individual2: Individual): Array[Individual] = {
    val crossoverPoint = if (Random.nextFloat < crossoverRate) Random.nextInt(target.length) else target.length
    val s1 = individual1.s.take(crossoverPoint) + individual2.s.drop(crossoverPoint)
    val s2 = individual2.s.take(crossoverPoint) + individual1.s.drop(crossoverPoint)

    Array(
      Individual(s1.map(mutate)),
      Individual(s2.map(mutate))
    )
  }

  def reproduceTwo(populationFitness: Array[Double]): Array[Individual] = {
    val individual1 = selectOne(populationFitness)
    val individual2 = selectOne(populationFitness)
    reproduce(individual1, individual2)
  }

  def next(): Unit = {
    individuals = Array.fill(individuals.length / 2)(reproduceTwo(fitness)).flatten
  }

  def population: List[Individual] = individuals.toList
}


object Population {

  def apply(populationSize: Int, crossoverRate: Float, mutationRate: Float, target: String): Population =
    new Population(populationSize, crossoverRate, mutationRate, target)
}

case class GAStringConfig(target: String, mutationRate: Float, crossoverRate: Float, populationSize: Int, showTop: Int)

object GAStringConfigs {
  val corgis = GAStringConfig(
    "welsh corgis have historically been used as herding dogs specifically for cattle",
    0.01f,
    0.75f,
    500,
    5
  )
}

class GAString extends PApplet {

  val config = GAStringConfigs.corgis

  val population = Population(config.populationSize, config.crossoverRate, config.mutationRate, config.target)

  override def settings(): Unit = {
    size(600, 600, PConstants.P2D)
  }

  override def setup(): Unit = {
    background(255)
    textSize(10)
    fill(0)
  }

  override def draw(): Unit = {
    population.next()
    if (frameCount % 5 == 0) {
      background(255)
      val top = population.population.sortBy(_.fitness(config.target)).reverse.take(config.showTop)
      top.zipWithIndex.foreach { case (ind, i) =>
        text(f"${i + 1}. '${ind.s}', Fitness:  ${ind.fitness(config.target)}%1.3f", 50, 50 + 100 * i)
      }
      if (top.head.s == config.target) noLoop()
    }
  }

}

object GAString extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.evolution.GAString")
  }
}
