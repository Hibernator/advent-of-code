package ch.hibernator.adventofcode

import scala.io.Source

object Puzzle12 extends App {
  private val source = Source.fromFile("input12.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val possibleDirections: Set[Int] = Set(0, 90, 180, 270)

  val moves = input.map(parseLine)

  val finalState = moves.foldLeft(State(0, 0, 90)) { case (currentState, move) =>
    move.move(currentState)
  }

  val manhattanDistance = finalState.x.abs + finalState.y.abs
  println(manhattanDistance)

  def parseLine(line: String): Move = line.head match {
    case 'N' => MoveNorth(line.tail.toInt)
    case 'E' => MoveEast(line.tail.toInt)
    case 'S' => MoveSouth(line.tail.toInt)
    case 'W' => MoveWest(line.tail.toInt)
    case 'L' => TurnLeft(line.tail.toInt)
    case 'R' => TurnRight(line.tail.toInt)
    case 'F' => MoveForward(line.tail.toInt)
    case _ => throw new RuntimeException(s"Wrong definition of the move: $line")
  }

  case class State(x: Int, y: Int, direction: Int) {
    assert(possibleDirections.contains(direction))
  }

  sealed trait Move {
    def move(currentState: State): State
  }
  case class MoveNorth(distance: Int) extends Move {
    override def move(currentState: State): State = currentState.copy(y = currentState.y - distance)
  }
  case class MoveSouth(distance: Int) extends Move {
    override def move(currentState: State): State = currentState.copy(y = currentState.y + distance)
  }
  case class MoveWest(distance: Int) extends Move {
    override def move(currentState: State): State = currentState.copy(x = currentState.x - distance)
  }
  case class MoveEast(distance: Int) extends Move {
    override def move(currentState: State): State = currentState.copy(x = currentState.x + distance)
  }
  case class MoveForward(distance: Int) extends Move {
    override def move(currentState: State): State = currentState.direction match {
      case 0 => MoveNorth(distance).move(currentState)
      case 90 => MoveEast(distance).move(currentState)
      case 180 => MoveSouth(distance).move(currentState)
      case 270 => MoveWest(distance).move(currentState)
      case _ => throw new RuntimeException(s"Wrong direction: ${currentState.direction}")
    }
  }
  case class TurnLeft(degrees: Int) extends Move {
    override def move(currentState: State): State = {
      val newDirection = currentState.direction - degrees
      currentState.copy(direction = (if (newDirection < 0) newDirection + 360 else newDirection) % 360)
    }
  }
  case class TurnRight(degrees: Int) extends Move {
    override def move(currentState: State): State =
      currentState.copy(direction = (currentState.direction + degrees) % 360)
  }
}
