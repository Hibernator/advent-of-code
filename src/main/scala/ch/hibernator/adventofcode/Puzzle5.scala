package ch.hibernator.adventofcode

import scala.io.Source

object Puzzle5 extends App {

  private val source = Source.fromFile("input5.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  println(input)

  val rowNumbers =
    input.map(line => decodeRowOrSeat('F', 'B', (0, 127), line.take(7)))
  val seatNumbers =
    input.map(line => decodeRowOrSeat('L', 'R', (0, 7), line.takeRight(3)))
  val rowsAndSeats = rowNumbers.zip(seatNumbers)
  val uniqueSeatNumbers = rowNumbers
    .zip(seatNumbers)
    .map {
      case (row, seat) =>
        row * 8 + seat
    }
    .sorted

  uniqueSeatNumbers.init.zip(uniqueSeatNumbers.tail).foreach {
    case (front, back) =>
      if (back - front != 1) println(front + 1)
  }

  def decodeRowOrSeat(
      lowCharacter: Char,
      highCharacter: Char,
      range: (Int, Int),
      encodedRow: String
  ): Int = {
    val finalRange = encodedRow.foldLeft(range) {
      case (previousRange, step) =>
        val (low, high) = previousRange
        step match {
          case x if x == lowCharacter =>
            (low, low + (high - low) / 2)
          case x if x == highCharacter => (low + (high - low) / 2 + 1, high)
          case _                       => throw new IllegalAccessException("Wrong character")
        }
    }
    assert(finalRange._1 == finalRange._2)
    finalRange._1
  }

//  println(rowNumbers)
//  println(seatNumbers)
//  println(uniqueSeatNumbers.sorted)
//  println(uniqueSeatNumbers.max)
}
