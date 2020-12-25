package ch.hibernator.adventofcode

import scala.io.Source

object Puzzle12Second extends App {
  private val source = Source.fromFile("input12.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val moves = input.map(parseLine)

  val initialState = State(Position(0, 0), Position(10, 1))
  val finalState = moves.foldLeft(initialState) { case (currentState, move) =>
    move.move(currentState)
  }

  val manhattanDistance = finalState.shipPosition.x.abs + finalState.shipPosition.y.abs
  println(manhattanDistance)

  def parseLine(line: String): Move = line.head match {
    case 'N' => MoveWaypointNorth(line.tail.toInt)
    case 'E' => MoveWaypointEast(line.tail.toInt)
    case 'S' => MoveWaypointSouth(line.tail.toInt)
    case 'W' => MoveWaypointWest(line.tail.toInt)
    case 'L' => TurnWaypointLeft(line.tail.toInt)
    case 'R' => TurnWaypointRight(line.tail.toInt)
    case 'F' => MoveForward(line.tail.toInt)
    case _ => throw new RuntimeException(s"Wrong definition of the move: $line")
  }

  case class Position(x: Int, y: Int)

  case class State(shipPosition: Position, waypointPosition: Position)

  sealed trait Move {
    def move(currentState: State): State
  }
  case class MoveWaypointNorth(distance: Int) extends Move {
    override def move(currentState: State): State = currentState.copy(
      waypointPosition = currentState.waypointPosition.copy(
        y = currentState.waypointPosition.y + distance))
  }
  case class MoveWaypointSouth(distance: Int) extends Move {
    override def move(currentState: State): State = currentState.copy(
      waypointPosition = currentState.waypointPosition.copy(
        y = currentState.waypointPosition.y - distance))
  }
  case class MoveWaypointWest(distance: Int) extends Move {
    override def move(currentState: State): State = currentState.copy(
      waypointPosition = currentState.waypointPosition.copy(
        x = currentState.waypointPosition.x - distance))
  }
  case class MoveWaypointEast(distance: Int) extends Move {
    override def move(currentState: State): State = currentState.copy(
      waypointPosition = currentState.waypointPosition.copy(
        x = currentState.waypointPosition.x + distance))
  }
  case class MoveForward(distance: Int) extends Move {
    override def move(currentState: State): State = {
      val xChange = currentState.waypointPosition.x * distance
      val yChange = currentState.waypointPosition.y * distance
      val newShipPosition = currentState.shipPosition.copy(
        x = currentState.shipPosition.x + xChange,
        y = currentState.shipPosition.y + yChange
      )
      currentState.copy(shipPosition = newShipPosition)
    }
  }
  case class TurnWaypointLeft(degrees: Int) extends Move {
    override def move(currentState: State): State =
      (currentState.waypointPosition.x, currentState.waypointPosition.y) match {
        case (xDiff, yDiff) if xDiff >= 0 && yDiff >= 0 => degrees match { //NE
          case 90 => currentState.copy(
            waypointPosition = Position(
              x = - currentState.waypointPosition.y,
              y = currentState.waypointPosition.x
            )
          )
          case 180 => currentState.copy(
            waypointPosition = Position(
              x = - currentState.waypointPosition.x,
              y = - currentState.waypointPosition.y
            )
          )
          case 270 => currentState.copy(
            waypointPosition = Position(
              x = currentState.waypointPosition.y,
              y = - currentState.waypointPosition.x
            )
          )
        }
        case (xDiff, yDiff) if xDiff >= 0 && yDiff <= 0 => degrees match { //SE
          case 90 => currentState.copy(
            waypointPosition = Position(
              x = - currentState.waypointPosition.y,
              y = currentState.waypointPosition.x
            )
          )
          case 180 => currentState.copy(
            waypointPosition = Position(
              x = - currentState.waypointPosition.x,
              y = - currentState.waypointPosition.y
            )
          )
          case 270 => currentState.copy(
            waypointPosition = Position(
              x = currentState.waypointPosition.y,
              y = - currentState.waypointPosition.x
            )
          )
        }
        case (xDiff, yDiff) if xDiff <= 0 && yDiff <= 0 => degrees match { //SW
          case 90 => currentState.copy(
            waypointPosition = Position(
              x = - currentState.waypointPosition.y,
              y = currentState.waypointPosition.x
            )
          )
          case 180 => currentState.copy(
            waypointPosition = Position(
              x = - currentState.waypointPosition.x,
              y = - currentState.waypointPosition.y
            )
          )
          case 270 => currentState.copy(
            waypointPosition = Position(
              x = currentState.waypointPosition.y,
              y = - currentState.waypointPosition.x
            )
          )
        }
        case (xDiff, yDiff) if xDiff <= 0 && yDiff >= 0 => degrees match { //NW
          case 90 => currentState.copy(
            waypointPosition = Position(
              x = - currentState.waypointPosition.y,
              y = currentState.waypointPosition.x
            )
          )
          case 180 => currentState.copy(
            waypointPosition = Position(
              x = - currentState.waypointPosition.x,
              y = - currentState.waypointPosition.y
            )
          )
          case 270 => currentState.copy(
            waypointPosition = Position(
              x = currentState.waypointPosition.y,
              y = - currentState.waypointPosition.x
            )
          )
        }
      }
  }
  case class TurnWaypointRight(degrees: Int) extends Move {
    override def move(currentState: State): State =
      TurnWaypointLeft(360 - degrees).move(currentState)
  }
}
