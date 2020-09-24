package beautyofnature.competitionandcooperation

import processing.core.{PApplet, PConstants}

import scala.util.Random

trait Strategy {
  def move(lastOtherMove: Int, lastOwnMove: Int): Int
  def displayName: String
}

object Strategy {
  val COOPERATE = 0
  val DEFECT = 1

  def flip(v: Int): Int = if (v == COOPERATE) DEFECT else COOPERATE

  def random: Int = Random.nextInt(2)
}

case object AlwaysCooperate extends Strategy {
  override def move(lastOtherMove: Int, lastOwnMove: Int): Int = Strategy.COOPERATE

  override def displayName: String = "Always Cooperate"
}

case object TitForTat extends Strategy {
  override def move(lastOtherMove: Int, lastOwnMove: Int): Int = lastOtherMove

  override def displayName: String = "Tit for Tat"
}

case class RandomStrategy(rcp: Float) extends Strategy {
  override def move(lastOtherMove: Int, lastOwnMove: Int): Int = if (Random.nextFloat() < rcp) 0 else 1

  override def displayName: String = "Random"
}

case object Pavlov extends Strategy {
  override def move(lastOtherMove: Int, lastOwnMove: Int): Int =
    if (lastOtherMove == Strategy.DEFECT) Strategy.flip(lastOwnMove) else lastOwnMove

  override def displayName: String = "Pavlov"
}

case object AlwaysDefect extends Strategy {
  override def move(lastOtherMove: Int, lastOwnMove: Int): Int = Strategy.DEFECT

  override def displayName: String = "Always Defect"
}

case class SpatialIteratedPrisonersDilemmaConfig(w: Int, h: Int, rounds: Int, rcp: Float, noise: Float,
                                            CC: Int, CD: Int, DC: Int, DD: Int)

object SpatialIteratedPrisonersDilemmaConfigs {

  val default = SpatialIteratedPrisonersDilemmaConfig(400, 400, 5, 0.5f, 0.01f, 3, 0, 5, 1)

}

class SpatialIteratedPrisonersDilemma extends PApplet {

  val config = SpatialIteratedPrisonersDilemmaConfigs.default

  val w = config.w
  val h = config.h
  val rounds = config.rounds
  val CC = config.CC
  val CD = config.CD
  val DC = config.DC
  val DD = config.DD
  val rcp = config.rcp
  val noise = config.noise

  val INT2POS = Array(
    (-1, 0),
    (-1, 1),
    (0, 1),
    (1, 1)
  )

  def posFromInt(i: Int): (Int, Int) = INT2POS(i)

  val POS2INT = Array(
    Array(5, 3, 0),
    Array(6, -1, 1),
    Array(7, 4, 2)
  )

  def intFromPos(i: Int, j: Int): Int = POS2INT(i + 1)(j + 1)

  val availableStrategies = Array(AlwaysCooperate, TitForTat, RandomStrategy(rcp), Pavlov, AlwaysDefect)

  def randomStrategy: Strategy = availableStrategies(Random.nextInt(availableStrategies.length))

  val scores = Array.fill(h, w)(0.0f)
  var strategies = Array.fill(h, w)(randomStrategy)
  var actions = Array.fill(h,w)(Strategy.COOPERATE)
  var lastActions = Array.fill(h, w)(Strategy.COOPERATE)
  var newStrategies = strategies

  override def settings(): Unit = {
    size(w, h + 100, PConstants.P2D)
  }

  override def setup(): Unit = {
    colorMode(PConstants.HSB, 360, 100, 100)
    frameRate = 1
  }

  def getBit(num: Int, bit: Int): Int = if (((1 << bit) & num) == 0) 0 else 1

  def setBit(num: Int, bit: Int, value: Int): Int = if (value == 1) num | (1 << bit) else num & ~(1 << bit)

  def payoff(s1: Int, s2: Int): Int = (s1, s2) match {
    case (Strategy.DEFECT, Strategy.DEFECT) => DD
    case (Strategy.DEFECT, Strategy.COOPERATE) => DC
    case (Strategy.COOPERATE, Strategy.COOPERATE) => CC
    case _ => CD
  }

  def updateCell(i: Int, j: Int): Unit = {
    (0 until 4).foreach { wi =>
      val (k, l) = posFromInt(wi)

      val ii = (i + w + k) % w
      val jj = (j + h + l) % h

      val prev1 = getBit(lastActions(j)(i), intFromPos(k, l))
      val prev2 = getBit(lastActions(jj)(ii), intFromPos(-k, -l))

      val action1 = if (Random.nextFloat < noise) Strategy.random else strategies(j)(i).move(prev2, prev1)
      val action2 = if (Random.nextFloat < noise) Strategy.random else strategies(jj)(ii).move(prev1, prev2)

      scores(j)(i) += payoff(action1, action2)
      scores(jj)(ii) += payoff(action2, action1)

      actions(j)(i) = setBit(actions(j)(i), intFromPos(k, l), action1)
      actions(jj)(ii) = setBit(actions(jj)(ii), intFromPos(-k, -l), action2)
    }
  }

  def getColor(s: Strategy): Int = s match {
    case AlwaysCooperate => color(120, 100, 100) // green
    case AlwaysDefect => color(0, 100, 100) // red
    case TitForTat => color(300, 100, 100) // purple
    case _: RandomStrategy => color(60, 100, 100) // yellow
    case Pavlov => color(180, 100, 100) // blue
  }

  def bestStrategy(i: Int, j: Int): Strategy = {
    val idxs = for {
      k <- -1 to 1
      l <- -1 to 1
    } yield {
      ((i + w + k) % w, (j + h + l) % h)
    }
    val ((ii, jj), b) = idxs.foldLeft(((0, 0), -1f)){ case (((bestI, bestJ), bestScore), (ic, jc)) =>
      if (scores(jc)(ic) > bestScore) ((ic, jc), scores(jc)(ic)) else ((bestI, bestJ), bestScore)
    }
    strategies(jj)(ii)
  }

  def update(): Unit = {
    for {
      j <- 0 until h
      i <- 0 until w
    } {
      updateCell(i, j)
    }
  }

  def update(n: Int): Unit = {
    (0 until n).foreach { _ =>
      update()
      val temp = actions
      actions = lastActions
      lastActions = temp
    }
  }

  def reset(): Unit = {
    for {
      j <- 0 until h
      i <- 0 until w
    } {
      actions(j)(i) = Strategy.COOPERATE
      lastActions(j)(i) = Strategy.COOPERATE
      scores(j)(i) = 0.0f
    }
  }

  def showStrategies(): Unit = {
    for {
      j <- 0 until h
      i <- 0 until w
    } {
      val c = getColor(strategies(j)(i))
      stroke(c)
      point(i, j)
    }
  }

  def updateStrategies(): Unit = {
    for {
      j <- 0 until h
      i <- 0 until w
    } {
        newStrategies(j)(i) = bestStrategy(i, j)
    }
    val temp = strategies
    strategies = newStrategies
    newStrategies = temp
  }

  def showStats(): Unit = {
    pushStyle()
    noStroke()
    val valueCount = strategies.flatten.groupBy(_.displayName).map {
      case (k, values) => (k, values.length)
    }
    availableStrategies.zipWithIndex.foreach { case (s, i) =>
      fill(getColor(s))
      val y = h + 30 + 15 * i
      circle(20, y - 3, 6)
      fill(0)
      val name = s.displayName
      text(f"$name: ${(valueCount.getOrElse(name, 0) * 1f) / (w * h)}%1.3f", 30, y)
    }
    popStyle()
  }

  override def draw(): Unit = {
    background(360)
    reset()
    showStrategies()
    showStats()
    update(rounds)
    updateStrategies()
  }

}

object SpatialIteratedPrisonersDilemma extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.competitionandcooperation.SpatialIteratedPrisonersDilemma")
  }
}
