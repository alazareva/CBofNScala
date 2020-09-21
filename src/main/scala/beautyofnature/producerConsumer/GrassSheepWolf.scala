package beautyofnature.producerConsumer

import processing.core.PApplet

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
}
case class FullGridCell(species: Species, visited: Boolean) extends GridCell{
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
case class EmptyGridCell(energy: Float) extends GridCell{

  override def isEmpty: Boolean = true

  override def isGrass: Boolean = false

  override def isSheep: Boolean = false

  override def isWolf: Boolean = false
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
                          initialSheep: Int = 500,
                          initialWolf: Int = 50,
                          minPlants: Int = 3,
                         )

case class Game(board: Array[GridCell], parameters: GameParameters) {

  def makeNewGrass(i: Int, j: Int): Boolean = ???

  def updateGrass(): Unit = {
    for {
      i <- 0 until parameters.row
      j <- 0 until parameters.cols
    } board(i)(j) match {
      case EmptyGridCell(e) if e >= parameters.grassEnergy  => {
        if (makeNewGrass(i, j)) board(i)(j) = FullGridCell(Grass(parameters.grassEnergy), visited = true)
      }
      case _ => ()
    }
    for {
      i <- 0 until parameters.row
      j <- 0 until parameters.cols
    } board(i)(j) match {
      case FullGridCell(g: Grass, true) =>  board(i)(j) = FullGridCell(g, visited = false)
      case EmptyGridCell(e) => board(i)(j) = EmptyGridCell(e + 1)
      case _ => ()
    }
  }

  def indexOfNeighbors(i: Int, j: Int)(predicate: GridCell => Boolean): List[(Int, Int)] = {
    val matches = for {
      ni <- i - 1 to i + 1
      nj <- j - 1 to j + 1
      if ni != nj && ni >= 0 && ni < parameters.row && nj >= 0 && nj < parameters.cols && predicate(board(ni)(nj))
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
            if (newSheep.energy > parameters.sheepEnergy) {
              board(ni)(nj) = FullGridCell(Sheep(newSheep.energy / 2), visited = true)
              board(i)(j) = FullGridCell(Sheep(newSheep.energy / 2), visited = true)
            } else {
              board(ni)(nj) = FullGridCell(Sheep(newSheep.energy + parameters.grassEnergy), visited = true)
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
        randomNeighborIndex(i, j)(_.isSheep) match {
          case Some((ni, nj)) =>
            if (newWolf.energy > parameters.wolfEnergy) {
              board(ni)(nj) = FullGridCell(Wolf(newWolf.energy / 2), visited = true)
              board(i)(j) = FullGridCell(Wolf(newWolf.energy / 2), visited = true)
            } else {
              board(ni)(nj) = FullGridCell(Wolf(newWolf.energy + parameters.sheepEnergy), visited = true)
              board(i)(j) = EmptyGridCell(0)
            }
          case None => moveToRandomEmptyCell(i, j, newWolf)
        }
      case _ => ()
    }
  }

}

class GrassSheepWolf extends PApplet {


}

object GrassSheepWolf extends PApplet {

  def main(args: Array[String]): Unit = {
    PApplet.main("beautyofnature.producerConsumer.GrassSheepWolf")
  }
}
