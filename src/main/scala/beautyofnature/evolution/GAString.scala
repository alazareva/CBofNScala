package beautyofnature.evolution


import processing.core.{PApplet, PConstants}

import scala.util.Random

case class StringIndividual(s: String) extends Individual

class StringPopulation(val populationSize: Int,
  crossoverRate: Float,
  mutationRate: Float,
  target: String) extends Population[StringIndividual] {

  override var individuals: Array[StringIndividual] = Array.fill(populationSize)(randomIndividual)

  override def individualFitness(individual: StringIndividual): Double = {
    val fit = individual.s.zip(target).foldLeft(0) { case (f, (c1, c2)) => if (c1 == c2) f + 1 else f }
    Math.pow(2, fit - target.length)
  }

  override def fitness: Array[Double] = {
    val rawFitness = individuals.map(individualFitness)
    val total = rawFitness.sum
    if (total == 0) rawFitness else rawFitness.map(_ / total)
  }

  override def randomIndividual: StringIndividual =
    StringIndividual((0 until target.length).map(_ => randomLetterOrSpace).mkString)

  def randomLetterOrSpace: Char = {
    val low = 'a'.toInt
    val high = 'z'.toInt + 2
    val ord = Random.nextInt(high - low) + low
    if (ord > 'z'.toInt) ' ' else ord.toChar
  }

  def mutate(c: Char): Char = if (Random.nextFloat < mutationRate) randomLetterOrSpace else c

  override def reproduceTwo(individual1: StringIndividual, individual2: StringIndividual): Array[StringIndividual] = {
    val crossoverPoint = if (Random.nextFloat < crossoverRate) Random.nextInt(target.length) else target.length
    val s1 = individual1.s.take(crossoverPoint) + individual2.s.drop(crossoverPoint)
    val s2 = individual2.s.take(crossoverPoint) + individual1.s.drop(crossoverPoint)

    Array(
      StringIndividual(s1.map(mutate)),
      StringIndividual(s2.map(mutate))
    )
  }
}

object StringPopulation {
  def apply(populationSize: Int, crossoverRate: Float, mutationRate: Float, target: String) =
    new StringPopulation(populationSize, crossoverRate, mutationRate, target)
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

  val population = StringPopulation(config.populationSize, config.crossoverRate, config.mutationRate, config.target)

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
      val top = population.population.sortBy(i => population.individualFitness(i)).reverse.take(config.showTop)
      top.zipWithIndex.foreach { case (ind, i) =>
        text(f"${i + 1}. '${ind.s}', Fitness:  ${population.individualFitness(ind)}%1.3f", 50, 50 + 100 * i)
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
