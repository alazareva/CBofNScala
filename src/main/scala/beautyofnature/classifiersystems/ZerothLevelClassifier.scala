package beautyofnature.classifiersystems

import processing.core.{PApplet, PConstants}

import scala.util.Random


case class Classifier(strength: Double, condition: String, action: String)

sealed trait WorldCell

case object Food extends WorldCell
case object Empty extends WorldCell
case object Rock extends WorldCell
case object Agent extends WorldCell


case class World(string: String, h: Int, w: Int)

object Worlds {
  val default = World(
                  """|...........O.........................OO.........O.........
                  |.OFO.......F........F.........O.......F.........FO........
                  |...........O........OO.......F............................
                  |............................O......O.........F......O.....
                  |...F.......OFO........OFO..........F.........OO......F....
                  |...OO..............................O..................O...
                  |..........OO.......O........OO....................O.......
                  |.OFO.......F.......OF........F.........OFO........F.......
                  |..................................................O.......
                  |...OO.......O.................O........OO.......O.........
                  |...F.......F.......O.........FO........F........OF....OFO.
                  |...........O.......OF.....................................
                  |..O......................O............O......O.......O....
                  |..F.......F...............F..........FO.......F......OF...
                  |..O.......OO...............O...................O..........
                  |.....................O.....................O..............
                  |...F........OFO......F..........F.........F......OF.......
                  |...OO................O..........OO.......O.......O........""".stripMargin, 18, 58)
}

class ZerothLevelClassifier extends PApplet {

  val scale = 20

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
  val reportEvery = 50
  var episodeReport = List.empty[Int]

  var oldActionList = List.empty[Int]

  val worldConfig = Worlds.default

  val classifiers: Array[Classifier] = Array.fill(size)(Classifier(initialStrength, randomCondition, randomAction))
  val world: Array[Array[WorldCell]] = worldFromString(worldConfig.string)

  val worldH = worldConfig.h
  val worldW = worldConfig.w

  var agentPosition: (Int, Int) = (-1, -1)

  val order = List((0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1))

  var stepCount = 0
  var episodeCount = 0

  override def settings(): Unit = {
    size(worldW * scale, worldH * scale + 200, PConstants.P2D)
  }

  def charToCell(c: Char): WorldCell = c match {
    case 'O' => Rock
    case 'F' => Food
    case _ => Empty
  }

  def worldFromString(str: String): Array[Array[WorldCell]] = {
    str.split("\n").map(_.toCharArray.map(charToCell))
  }

  def placeAgent(): Unit = {
    val indexes = for {
      i <- 0 until worldW
      j <- 0 until worldH
    } yield (i, j)
     Random.shuffle(indexes).find{case (i, j) => world(j)(i) match {
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
      val c = Random.nextInt(3).toString
      if (c == "2") "#" else c
    }).mkString
  }

  def compareStrings(s1: String, s2: String): Boolean =
    s1.zip(s2).forall{case (c1, c2) => c1 == '#' || c2 == '#' || c1 == c2}

  def randomAction: String = (0 until ALEN).map(_ => (Math.abs(Random.nextInt) % 2).toString).mkString

  def environment: String = {
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

  def covering(matches: List[Int], environment: String): List[Int] = {
    val totalStrength = matches.map(i => classifiers(i).strength).sum
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
      replace :: matches
    }
  }

  // All classifiers that match thee environment
  def matchList(env: String): List[Int] = {
    classifiers.zipWithIndex.collect{case (c, i) if compareStrings(c.condition, env) => i}.toList
  }

  def getActions(matches: List[Int]): List[Int] = {
    val totalStrength = matches.map(i => classifiers(i).strength).sum
    val prob = Random.nextFloat
    def rec(runningSum: Double, remaining: List[Int]): Classifier = remaining match {
      case head :: Nil => classifiers(head)
      case head :: _ if prob < runningSum + classifiers(head).strength / totalStrength => classifiers(head)
      case head :: tail => rec(runningSum + classifiers(head).strength / totalStrength, tail)
    }
    val pick = rec(0, matches)
    matches.filter(i => classifiers(i).action == pick.action)
  }

  def moveAgent(newX: Int, newY: Int): Unit = {
    val (agentX, agentY) = agentPosition
    world(agentY)(agentX) = Empty
    agentPosition = (newX, newY)
    world(newY)(newX) = Agent
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

  def update(reward: Int, matchList: List[Int], actionList: List[Int], oldActionList: List[Int]) = {
    val hold = actionList.map(i => classifiers(i).strength * learningRate).sum
    actionList.foreach(i => {
      val c = classifiers(i)
      val updated = c.copy(strength = c.strength + (-c.strength + reward / actionList.length) * learningRate)
      classifiers(i) = updated
    })
    oldActionList.foreach(i => {
      val c = classifiers(i)
      val updated = c.copy(strength = c.strength + discountRate * hold / oldActionList.length)
      classifiers(i) = updated
    })
    matchList.foreach(i => {
      val c = classifiers(i)
      val updated = if (c.action == classifiers(actionList.head).action) c else c.copy(strength = (1 - taxRate) * c.strength)
      classifiers(i) = updated
    })
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

    // cross the two children
    if (Random.nextFloat < crossoverRate) {
      if (Random.nextInt(CLEN + ALEN) < CLEN) { // cross conditions
        val crossoverIndex = Random.nextInt(CLEN)
        val c1 = classifiers(other1)
        val c2 = classifiers(other2)
        val new1 = c1.condition.take(crossoverIndex) + c2.condition.drop(crossoverIndex)
        val new2 = c2.condition.take(crossoverIndex) + c1.condition.drop(crossoverIndex)
        classifiers(other1) = c1.copy(condition = new1)
        classifiers(other2) = c2.copy(condition = new2)
      } else { // cross actions
        val crossoverIndex = Random.nextInt(ALEN)
        val c1 = classifiers(other1)
        val c2 = classifiers(other2)
        val new1 = c1.action.take(crossoverIndex) + c2.action.drop(crossoverIndex)
        val new2 = c2.action.take(crossoverIndex) + c1.action.drop(crossoverIndex)
        classifiers(other1) = c1.copy(action = new1)
        classifiers(other2) = c2.copy(action = new2)
      }
      val c1 = classifiers(other1)
      val c2 = classifiers(other2)
      val averageStrength = (c1.strength + c2.strength) / 2
      classifiers(other1) = c1.copy(strength = averageStrength)
      classifiers(other2) = c2.copy(strength = averageStrength)
    }

    // mutate conditions
    val class1 = classifiers(other1)
    val class2 = classifiers(other2)

    def mutateConditionChar(c: Char): String = {
      if (Random.nextFloat < mutationRate) {
      val r = Random.nextInt(3).toString
      if (r == "2") "#" else r
      } else c.toString
    }
    val cond1 = class1.condition.flatMap(mutateConditionChar)
    val cond2 = class2.condition.flatMap(mutateConditionChar)

    def mutateActionChar(c: Char): String = {
      if (Random.nextFloat < mutationRate) Random.nextInt(2).toString
      else c.toString
    }

    val act1 = class1.action.flatMap(mutateActionChar)
    val act2 = class2.action.flatMap(mutateActionChar)

    classifiers(other1) = class1.copy(condition=cond1, action=act1)
    classifiers(other2) = class2.copy(condition=cond2, action=act2)
  }

  def drawWorld(): Unit = {
    for {
      h <- 0 until  worldH
      w <- 0 until worldW
    } {
      val c = world(h)(w) match {
        case Empty => color(255, 255, 255)
        case Agent => color(255, 0, 0)
        case Food => color(0, 255, 0)
        case Rock => 0
      }
      fill(c)
      circle(scale / 2 + w * scale, scale / 2  + h * scale, scale)
    }
  }

  def restart(): Unit = {
    println("Restarting")
    val (agentX, agentY) = agentPosition
    world(agentY)(agentX) = Food
    placeAgent()
    episodeCount += 1
    if (episodeCount % reportEvery == 0) {
      episodeReport = stepCount / reportEvery :: episodeReport
      stepCount = 0
    }
  }

  def updateWorld(): Unit = {
    val env = environment
    val mList = covering(matchList(env), env)
    val actionList = getActions(mList)
    val reward = move(classifiers(actionList.head).action)
    update(reward, mList, actionList, oldActionList)
    if (Random.nextFloat < gaInvocationRate) geneticAlgorithm()
    oldActionList = actionList
    if (reward != 0) restart()
  }

  override def setup(): Unit = {
    placeAgent()
    frameRate = 1
    smooth()
  }

  def drawReport(): Unit = {
    pushStyle()
    fill(0)
    text(f"Average Moves per Episode", 20, worldH * scale + 20)
    episodeReport.take(10).zipWithIndex.foreach { case (r, i) =>
      text(f"$r", 20, worldH * scale + 40 + 20 * i)
    }
    text(f"Total Episodes: $episodeCount", 300, worldH * scale + 20)
    popStyle()
  }

  def displayClassifier(c: Classifier, i: Int, j: Int): Unit = {
    pushStyle()
    def getColor(char1: Char, char2: Char): Int = (char1, char2) match {
      case ('#', _) => color(100) // wildcard
      case (_, '#') => color(100) // wildcard
      case ('0', _) => color(255) // empty
      case ('1', '1') => color(0, 255, 0) // food
      case ('1', '0') => color(0) // rock
    }

    val actionIndex = c.action.foldLeft(0){ case (acc, c) => acc * 2 + (c - '0')}

    val condition = c.condition
    (0 until CLEN by 2).foreach { idx =>
      val emptyStatus = condition(idx)
      val foodStatus = condition(idx + 1)
      val (offX, offY) = order(idx/2)
      val color = getColor(emptyStatus, foodStatus)
      if (actionIndex == idx/2) strokeWeight(3) else strokeWeight(1)
      fill(color)
      circle(i + offX * scale, j + offY * scale, scale)
    }
   popStyle()
  }

  def drawTopClassifiers(): Unit = {
    fill(0)
    text("Top Policies", 500, worldH * scale + 20)
    val h = 2
    val w = 4
    val top = classifiers.sortBy(_.strength).reverse.take(h * w)
    for {
      i <- 0 until w
      j <- 0 until h
    } {
      val c = top(j * i)
      displayClassifier(c, 650 + i * 150,  worldH * scale + 40 + j * 100)
    }
  }

  override def draw(): Unit = {
    if (frameCount % 1 == 0) {
      background(255)
      drawWorld()
      drawReport()
      drawTopClassifiers()
      updateWorld()
      stepCount += 1
    }
  }
}

object ZerothLevelClassifier extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.classifiersystems.ZerothLevelClassifier")
  }
}
