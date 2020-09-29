package beautyofnature.classifierSystems

import processing.core.PApplet

import scala.util.Random


case class Classifier(strength: Double, condition: String, action: String)

sealed trait WorldCell

case object Food extends WorldCell
case object Empty extends WorldCell
case object Rock extends WorldCell
case object Agent extends WorldCell

class ZerothLevelClassifier extends PApplet {

  val worldW = 100
  val worldH = 100

  val CLEN = 16
  val ALEN = 3
  val size = 400
  val initialStrength = 20
  val cover = 0.5f
  val wild = 0.33f
  val gaInvocationRate = 0.25f
  val mutationRate = 0.002f
  val crossoverRate = 0.5f
  val taxRate = 0.1f
  val averageWindowLength = 50
  val learningRate = 0.2f
  val discountRate = 0.71f
  val foodReward = 1000


  val classifiers: Array[Classifier] = Array.fill(size)(Classifier(initialStrength, randomCondition, randomAction))
  val world: Array[Array[WorldCell]] = ???

  var agentPosition: (Int, Int) = (-1, -1)

  val order = List((0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1))


  def placeAgent(): Unit = {
    val indexes = for {
      i <- 0 until worldW
      j <- 0 until worldH
    } yield (i, j)
     indexes.find{case (i, j) => world(j)(i) match {
       case Empty => true
       case _ => false
     }} match {
       case Some((x, y)) => {
         world(y)(x) = Agent
         agentPosition = (x, y)
       }
       case _ => throw new RuntimeException("No empty cells")
     }
  }

  def emptyChar(c: WorldCell): Char = c match {
    case Empty => '0'
    case _ => '1'
  }

  def foodChar(c: WorldCell): Char = c match {
    case Food => '1'
    case _ => '0'
  }

  def randomCondition: String = {
    (0 until CLEN).map(_ => {
      val c = (Random.nextInt % 3).toChar
      if (c == '2') '#' else c
    }).mkString
  }

  def compareStrings(s1: String, s2: String): Boolean =
    s1.zip(s2).forall{case (c1, c2) => c1 == '#' || c2 == '#' || c1 == c2}

  def randomAction: String = (0 until ALEN).map(_ => (Random.nextInt % 2).toChar).mkString

  def getAgentPositionString: String = {
    val (agentX, agentY) = agentPosition
    order.map { case (xOff, yOff) =>
        val x = (agentX + xOff + worldW) % worldW
        val y = (agentY + yOff + worldH) % worldH
      s"${emptyChar(world(y)(x)) }${foodChar(world(y)(x))}"
    }.mkString
  }

  def pickRandomLargeClassifier(skip: Int): Int = {
    val totalStrength = classifiers.zipWithIndex.collect {
      case (c, i) if i != skip =>  c.strength
    }.sum
    val prob = Random.nextFloat
    def rec(runningSum: Double, idx: Int): Int = {
      if (idx >= classifiers.length) idx
      else if (idx == skip) rec(runningSum, idx + 1)
      else if (prob < runningSum + classifiers(idx).strength / totalStrength) idx
      else  rec(runningSum + classifiers(idx).strength / totalStrength, idx + 1)
    }
    rec(0, 0)
  }

  def pickRandomSmallClassifier(skip: Int): Int = {
    val inverseStrength = classifiers.zipWithIndex.collect {
      case (c, i) if i != skip =>  1 / c.strength
    }.sum
    val prob = Random.nextFloat
    def rec(runningSum: Double, idx: Int): Int = {
      if (idx >= classifiers.length) idx
      else if (idx == skip) rec(runningSum, idx + 1)
      else if (prob < runningSum + (1 / classifiers(idx).strength) / inverseStrength) idx
      else  rec(runningSum + (1 / classifiers(idx).strength) / inverseStrength, idx + 1)
    }
    rec(0, 0)
  }

  def covering(matches: List[Classifier], environment: String): List[Classifier] = {
    val totalStrength = matches.map(_.strength).sum
    val meanStrength = classifiers.map(_.strength).sum / classifiers.length
    if (totalStrength > meanStrength * cover) matches
    else {
      val replace = pickRandomSmallClassifier(-1)
      val newClassifier = Classifier(
        meanStrength,
        environment.map(c => if (Random.nextFloat < wild) '#' else c),
        randomAction
      )
      classifiers(replace) = newClassifier
      newClassifier :: matches
    }
  }

  def getActions(matches: List[Classifier]): List[Classifier] = {
    val totalStrength = matches.map(_.strength).sum
    val prob = Random.nextFloat
    def rec(runningSum: Double, remaining: List[Classifier]): Classifier = remaining match {
      case head :: Nil => head
      case head :: _ if prob < runningSum + head.strength / totalStrength => head
      case head :: tail => rec(runningSum + head.strength / totalStrength, tail)
    }
    val pick = rec(0, matches)
    matches.filter(_.action == pick.action)
  }

  def moveAgent(newX: Int, newY: Int): Unit = {
    val (agentX, agentY) = agentPosition
    world(agentY)(agentX) = Empty
    agentPosition = (newX, newY)
    worldW(newY)(newX) = Agent
  }

  def move(action: String): Int = {
    val (agentX, agentY) = agentPosition
    val index = action.foldLeft(0){ case (acc, c) => acc * 2 + (c - '0')}
    val newX = (agentX + order(index)._1 + worldW) % worldW
    val newY = (agentY + order(index)._2 + worldH) % worldH
    world(newY)(newX) match {
      case Rock => 0
      case Food =>
        moveAgent(newX, newY)
        foodReward
      case _ =>
        moveAgent(newX, newY)
        0
    }
  }

  def update(reward: Int, matchList: List[Classifier], actionList: List[Classifier], oldActionList: List[Classifier]) = {
    val hold = actionList.map(_.strength * learningRate).sum
    val newActionList = actionList.map(
      c => c.copy(strength = c.strength  + (-c.strength + reward / actionList.length) *  learningRate)
    )
    val newOldActionList = oldActionList.map(c => c.copy(strength = c.strength + discountRate * hold / oldActionList.length))
    val newMatchList = matchList.map(
      c => if (c.action == actionList.head.action) c else c.copy(strength = (1 - taxRate) * c.strength)
    )
    (newMatchList, newActionList, newOldActionList)
  }

  def geneticAlgorithm() = {
    val parent1 = pickRandomLargeClassifier(-1)
    val parent2 = pickRandomLargeClassifier(parent1)

    val other1 = pickRandomSmallClassifier(-1)
    val other2 = pickRandomSmallClassifier(other1)

    val c1 = classifiers(parent1)
    val c2 = classifiers(parent2)

    classifiers(parent1) = c1.copy(strength = c1.strength / 2)
    classifiers(other1) = classifiers(parent1)
    classifiers(parent2) = c2.copy(strength = c2.strength / 2)
    classifiers(other2) = classifiers(parent2)

    // TODO the rest
  }

  override def draw(): Unit = {

  }


}
