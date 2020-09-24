package beautyofnature.competitionandcooperation

import scala.util.Random

trait Strategy {
  def move(lastOtherMove: Int, lastOwnMove: Int): Int
}

object Strategy {
  val COOPERATE = 0
  val DEFECT = 1
}

case object AlwaysCooperate extends Strategy {
  override def move(lastOtherMove: Int, lastOwnMove: Int): Int = Strategy.COOPERATE
}

case object TitForTat extends Strategy {
  override def move(lastOtherMove: Int, lastOwnMove: Int): Int = lastOtherMove
}

case class Random(rcp: Float) extends Strategy {
  override def move(lastOtherMove: Int, lastOwnMove: Int): Int = if (Random.nextFloat < rcp) 0 else 1
}

case object Pavlov extends Strategy {
  override def move(lastOtherMove: Int, lastOwnMove: Int): Int =
    if (lastOtherMove == Strategy.DEFECT) ~lastOwnMove else lastOwnMove
}

case object AlwaysDefect extends Strategy {
  override def move(lastOtherMove: Int, lastOwnMove: Int): Int = Strategy.DEFECT
}

class SpatialIteratedPrisonersDilemma extends App {

  val w = 200
  val h = 200
  val rounds = 5
  val CC = 3
  val CD = 0
  val DC = 5
  val DD = 1
  val rcp = 0.5

  val int2pos = Array(
    (-1, 0),
    (-1, 1),
    (0, 1),
    (1, 1)
  )

  def int2posI(i: Int): Int = int2pos(i)._1
  def int2posJ(i: Int): Int = int2pos(i)._2

  val pos2int = Array(
    Array(5, 3, 0),
    Array(6, -1, 1),
    Array(7, 4, 2)
  )

  def pos2int(i: Int, j: Int) = pos2int(i + 1)(j + 1)
}


