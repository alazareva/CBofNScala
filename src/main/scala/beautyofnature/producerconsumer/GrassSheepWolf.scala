package beautyofnature.producerconsumer

import processing.core.{PApplet, PConstants}

import scala.annotation.tailrec
import scala.util.Random

trait Species {
  def energy: Float
}

case class Grass(energy: Float) extends Species

case class Sheep(energy: Float) extends Species

case class Wolf(energy: Float) extends Species

trait GridCell {
  def isGrass: Boolean

  def isSheep: Boolean

  def isWolf: Boolean

  def isEmpty: Boolean

  def visited: Boolean
}

case class FullGridCell(species: Species, visited: Boolean) extends GridCell {
  def isGrass: Boolean = species match {
    case _: Grass => true
    case _ => false
  }

  def isSheep: Boolean = species match {
    case _: Sheep => true
    case _ => false
  }

  def isWolf: Boolean = species match {
    case _: Wolf => true
    case _ => false
  }

  def isEmpty: Boolean = false
}

case class EmptyGridCell(energy: Float) extends GridCell {

  override def isEmpty: Boolean = true

  override def isGrass: Boolean = false

  override def isSheep: Boolean = false

  override def isWolf: Boolean = false

  override def visited: Boolean = false
}

case class GameParameters(row: Int = 100,
                          cols: Int = 100,
                          grassEnergy: Float = 15f,
                          sheepEnergy: Float = 50f,
                          wolfEnergy: Float = 200f,
                          grassStepToGrow: Int = 0,
                          shepStepCost: Float = 5,
                          wolfStepConst: Float = 5,
                          initialGrass: Int = 5000,
                          initialSheep: Int = 250,
                          initialWolf: Int = 10,
                          minPlants: Int = 2,
                         )

class Game(val board: Array[Array[GridCell]], val parameters: GameParameters) {

  def makeNewGrass(i: Int, j: Int): Boolean = indexOfNeighbors(i, j)(c => c.isGrass && !c.visited) match {
    case Nil => false
    case list => list.length >= parameters.minPlants
  }

  def updateGrass(): Unit = {
    for {
      i <- 0 until parameters.row
      j <- 0 until parameters.cols
    } board(i)(j) match {
      case EmptyGridCell(e) if e >= parameters.grassEnergy && makeNewGrass(i, j) =>
        board(i)(j) = FullGridCell(Grass(parameters.grassEnergy), visited = true)
      case EmptyGridCell(e) => board(i)(j) = EmptyGridCell(e + 1)
      case _ => ()
    }
  }

  def indexOfNeighbors(i: Int, j: Int)(predicate: GridCell => Boolean): List[(Int, Int)] = {
    val matches = for {
      ni <- i - 1 to i + 1
      nj <- j - 1 to j + 1
      if (ni, nj) != (i, j) && ni >= 0 &&
        ni < parameters.row && nj >= 0 &&
        nj < parameters.cols && predicate(board(ni)(nj))
    } yield (ni, nj)
    matches.toList
  }

  def randomNeighborIndex(i: Int, j: Int)(predicate: GridCell => Boolean): Option[(Int, Int)] = {
    indexOfNeighbors(i, j)(predicate) match {
      case Nil => None
      case list => Some(list(Random.nextInt(list.size)))
    }
  }

  def moveToRandomEmptyCell(i: Int, j: Int, animal: Species): Unit = randomNeighborIndex(i, j)(_.isEmpty) match {
    case Some((ei, ej)) =>
      board(ei)(ej) = FullGridCell(animal, visited = true)
      board(i)(j) = EmptyGridCell(0)
    case None => ()
  }

  def updateSheep(): Unit = {
    for {
      i <- 0 until parameters.row
      j <- 0 until parameters.cols
    } board(i)(j) match {
      case FullGridCell(Sheep(e), false) if e - parameters.shepStepCost <= 0 => board(i)(j) = EmptyGridCell(0)
      case FullGridCell(Sheep(e), false) =>
        val newSheep = Sheep(e - parameters.shepStepCost)
        randomNeighborIndex(i, j)(_.isGrass) match {
          case Some((ni, nj)) =>
            val newEnergy = newSheep.energy + parameters.grassEnergy
            board(ni)(nj) = FullGridCell(Sheep(newEnergy), visited = true)
            if (newEnergy > parameters.sheepEnergy) {
              board(ni)(nj) = FullGridCell(Sheep(newEnergy / 2), visited = true)
              board(i)(j) = FullGridCell(Sheep(newEnergy / 2), visited = true)
            } else {
              board(i)(j) = EmptyGridCell(0)
            }
          case None => moveToRandomEmptyCell(i, j, newSheep)
        }
      case _ => ()
    }
  }

  def updateWolves(): Unit = {
    for {
      i <- 0 until parameters.row
      j <- 0 until parameters.cols
    } board(i)(j) match {
      case FullGridCell(Wolf(e), false) if e - parameters.wolfStepConst <= 0 => board(i)(j) = EmptyGridCell(0)
      case FullGridCell(Wolf(e), false) =>
        val newWolf = Wolf(e - parameters.wolfStepConst)
        randomNeighborIndex(i, j)(c => c.isSheep || c.isGrass) match {
          case Some((ni, nj)) =>
            val newEnergy = if (board(ni)(nj).isSheep) newWolf.energy + parameters.sheepEnergy else newWolf.energy
            board(ni)(nj) = FullGridCell(Wolf(newEnergy), visited = true)
            if (newEnergy > parameters.wolfEnergy) {
              board(ni)(nj) = FullGridCell(Wolf(newEnergy / 2), visited = true)
              board(i)(j) = FullGridCell(Wolf(newEnergy / 2), visited = true)
            } else {
              board(i)(j) = EmptyGridCell(0)
            }
          case None => moveToRandomEmptyCell(i, j, newWolf)
        }
      case _ => ()
    }
  }

  def updateVisitedFlag(): Unit = for {
    i <- 0 until parameters.row
    j <- 0 until parameters.cols
  } board(i)(j) match {
    case FullGridCell(a, true) => board(i)(j) = FullGridCell(a, visited = false)
    case _ => ()
  }

  def update(): (Int, Int, Int) = {
    updateVisitedFlag()
    updateGrass()
    updateSheep()
    updateWolves()
    board.flatten.foldLeft((0, 0, 0)) { case ((g, s, w), cell) => cell match {
      case _: EmptyGridCell => (g, s, w)
      case FullGridCell(_: Grass, _) => (g + 1, s, w)
      case FullGridCell(_: Sheep, _) => (g, s + 1, w)
      case FullGridCell(_: Wolf, _) => (g, s, w + 1)
    }
    }
  }
}

object Game {

  def emptyCells(board: Array[Array[GridCell]], rows: Int, cols: Int): List[(Int, Int)] = {
    val matches = for {
      i <- 0 until rows
      j <- 0 until cols
      if board(i)(j).isEmpty
    } yield (i, j)
    matches.toList
  }

  def apply(parameters: GameParameters): Game = {
    val board = Array.ofDim[GridCell](parameters.row, parameters.cols)
    for {
      i <- 0 until parameters.row
      j <- 0 until parameters.cols
    } board(i)(j) = EmptyGridCell(0)

    @tailrec
    def addToBoard(remGrass: Int, remSheep: Int, remWolves: Int, emptyCells: List[(Int, Int)]): Unit = {
      if ((remGrass == 0 && remSheep == 0 && remWolves == 0) || emptyCells.isEmpty) ()
      else {
        val (i, j) :: tail = emptyCells
        if (remGrass > 0) {
          board(i)(j) = FullGridCell(Grass(parameters.grassEnergy), visited = false)
          addToBoard(remGrass - 1, remSheep, remWolves, tail)
        } else if (remSheep > 0) {
          board(i)(j) = FullGridCell(Sheep(parameters.sheepEnergy), visited = false)
          addToBoard(remGrass, remSheep - 1, remWolves, tail)
        } else {
          board(i)(j) = FullGridCell(Wolf(parameters.wolfEnergy), visited = false)
          addToBoard(remGrass, remSheep, remWolves - 1, tail)
        }
      }
    }

    val randomCells = Random.shuffle(emptyCells(board, parameters.row, parameters.cols))
    addToBoard(parameters.initialGrass, parameters.initialSheep, parameters.initialWolf, randomCells)

    new Game(board, parameters)
  }
}

object GrassSheepWolfParameters {
  val default = GameParameters()
}

class GrassSheepWolf extends PApplet {

  val game = Game(GrassSheepWolfParameters.default)

  var counts: List[(Int, Int, Int)] = List.empty[(Int, Int, Int)]

  override def settings(): Unit = {
    size(800, 900, PConstants.P2D)
  }

  override def setup(): Unit = {
    background(255)
    colorMode(PConstants.RGB, 255, 255, 255, 100)
    frameRate(3)
    strokeWeight(0.5f)
    stroke(200)
    counts = (game.parameters.initialGrass, game.parameters.initialSheep, game.parameters.initialWolf) :: counts
  }

  def drawGrid(): Unit = {
    val diameter = width / game.parameters.row
    for {
      i <- 0 until game.parameters.row
      j <- 0 until game.parameters.cols
    } {
      val c = game.board(i)(j) match {
        case EmptyGridCell(e) => color(255, 255, 255, PApplet.map(e, 0, game.parameters.grassEnergy, 0, 50))
        case FullGridCell(Grass(e), _) => color(0, 255, 0, PApplet.map(e, 0, game.parameters.grassEnergy, 90, 100))
        case FullGridCell(Sheep(e), _) => color(0, 0, 255, PApplet.map(e, 0, game.parameters.sheepEnergy, 0, 100))
        case FullGridCell(Wolf(e), _) => color(255, 0, 0, PApplet.map(e, 0, game.parameters.wolfEnergy, 0, 100))
      }
      val x = PApplet.map(j, 0, game.parameters.cols, 0, width)
      val y = PApplet.map(i, 0, game.parameters.row, 0, width)
      fill(c)
      circle(x + diameter / 2, y + diameter / 2, diameter)
    }
  }

  def drawCounts(): Unit = {
    val toShow = counts.take(100)
    val (maxGrass, maxSheep, maxWolf) = toShow.foldLeft((0, 0, 0)) {
      case ((mg, ms, mw), (gr, s, w)) => (Math.max(mg, gr), Math.max(ms, s), Math.max(mw, w))
    }
    pushStyle()
    noFill()
    strokeWeight(1)
    stroke(0, 255, 0)
    beginShape()
    toShow.zipWithIndex.foreach { case (t, i) =>
      val x = PApplet.map(toShow.length - i, 0, 100, 0, width)
      val y = PApplet.map(t._1, 0, maxGrass, height - 5, height - 95)
      curveVertex(x, y)
    }
    endShape()

    stroke(0, 0, 255)
    beginShape()
    toShow.zipWithIndex.foreach { case (t, i) =>
      val x = PApplet.map(toShow.length - i, 0, 100, 0, width)
      val y = PApplet.map(t._2, 0, maxSheep, height - 5, height - 95)
      curveVertex(x, y)
    }
    endShape()

    stroke(255, 0, 0)
    beginShape()
    toShow.zipWithIndex.foreach { case (t, i) =>
      val x = PApplet.map(toShow.length - i, 0, 100, 0, width)
      val y = PApplet.map(t._3, 0, maxWolf, height - 5, height - 95)
      curveVertex(x, y)
    }
    endShape()
    popStyle()
  }

  override def draw(): Unit = {
    background(255)
    drawGrid()
    drawCounts()
    val newCounts = game.update()
    counts = newCounts :: counts
  }
}

object GrassSheepWolf extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.producerconsumer.GrassSheepWolf")
  }
}
