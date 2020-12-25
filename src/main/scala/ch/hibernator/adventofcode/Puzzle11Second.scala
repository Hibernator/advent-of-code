package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Puzzle11Second extends App {
  private val source = Source.fromFile("input11.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val maxRowIndex = input.size - 1
  val maxColumnIndex = input.head.length - 1

  val initialMap: mutable.Map[Position, Tile] =
    mutable.Map()

  input.zipWithIndex.foreach {
    case (row, rowIndex) =>
      row.zipWithIndex.foreach {
        case (tileChar, columnIndex) =>
          val tile = if (tileChar == 'L') Seat(isEmpty = true) else Floor
          initialMap.update(Position(rowIndex, columnIndex), tile)
      }
  }

  val directionChanges: Seq[DirectionChange] = Seq(
    DirectionChange(-1, 0),
    DirectionChange(-1, 1),
    DirectionChange(0, 1),
    DirectionChange(1, 1),
    DirectionChange(1, 0),
    DirectionChange(1, -1),
    DirectionChange(0, -1),
    DirectionChange(-1, -1)
  )

  val numMoves = finalOccupiedSeats(initialMap)
  println(numMoves)

  @tailrec
  def finalOccupiedSeats(currentState: mutable.Map[Position, Tile]): Int = {
    val newState = mutable.Map[Position, Tile]()

    def getVisibleSeats(position: Position) = {

      @tailrec
      def getVisibleSeat(position: Position, direction: DirectionChange): Option[Tile] = {
        val newPosition = position.copy(
          row = position.row + direction.rowChange,
          column = position.column + direction.columnChange)
        val maybeNextTile = currentState.get(newPosition)
        maybeNextTile match {
          case None => None
          case seat @ Some(Seat(_)) => seat
          case _ @ Some(Floor) => getVisibleSeat(newPosition, direction)
        }
      }

      directionChanges.flatMap(getVisibleSeat(position, _))
    }

    currentState.foreach { case (position, tile) =>
      tile match {
        case Floor => newState.update(position, tile)
        case seat: Seat =>
          val numVisibleOccupied = getVisibleSeats(position).count(!_.isEmpty)
          if (numVisibleOccupied == 0) newState.update(position, Seat(isEmpty = false))
          else if (numVisibleOccupied >= 5) newState.update(position, Seat(isEmpty = true))
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

  case class DirectionChange(rowChange: Int, columnChange: Int)
}
