package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Puzzle11 extends App {
  private val source = Source.fromFile("input11.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val initialMap: mutable.Map[Position, Tile] =
    mutable.Map().withDefaultValue(Floor)

  input.zipWithIndex.foreach {
    case (row, rowIndex) =>
      row.zipWithIndex.foreach {
        case (tileChar, columnIndex) =>
          val tile = if (tileChar == 'L') Seat(isEmpty = true) else Floor
          initialMap.update(Position(rowIndex, columnIndex), tile)
      }
  }

  val numMoves = finalOccupiedSeats(initialMap)
  println(numMoves)

  @tailrec
  def finalOccupiedSeats(currentState: mutable.Map[Position, Tile]): Int = {
    val newState = mutable.Map[Position, Tile]().withDefaultValue(Floor)

    def getAdjacentTiles(position: Position) = {
        for {
          rowIndex <- (position.row - 1) to (position.row + 1)
          columnIndex <- (position.column - 1) to (position.column + 1)
        } yield {
          val position = Position(rowIndex, columnIndex)
          (position, currentState(position))
        }
      }.filterNot(_._1 == position).map(_._2)

    currentState.foreach { case (position, tile) =>
      tile match {
        case Floor => newState.update(position, tile)
        case seat: Seat =>
          val numAdjacentOccupied = getAdjacentTiles(position).count(!_.isEmpty)
          if (numAdjacentOccupied == 0) newState.update(position, Seat(isEmpty = false))
          else if (numAdjacentOccupied >= 4) newState.update(position, Seat(isEmpty = true))
          else newState.update(position, seat)
      }
    }

    if (currentState == newState) currentState.values.count(!_.isEmpty)
    else finalOccupiedSeats(newState)

  }

  sealed abstract class Tile {
    def isEmpty: Boolean
  }
  case object Floor extends Tile {
    override val isEmpty: Boolean = true
  }
  case class Seat(override val isEmpty: Boolean) extends Tile

  case class Position(row: Int, column: Int)
}
