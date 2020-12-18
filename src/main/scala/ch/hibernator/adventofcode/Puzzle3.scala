package ch.hibernator.adventofcode

import scala.io.Source
import scala.util.chaining._

object Puzzle3 extends App {

  private val source = Source.fromFile("input.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val numRows = input.size
  val numCols = input.head.length

  val setOfTrees: Set[Coordinates] = (for {
    (row, rowNum) <- input.zipWithIndex
    (character, colNum) <- row.zipWithIndex if character == '#'
  } yield Coordinates(rowNum, colNum)).toSet

  val result = (countNumTrees(1, 1).toLong
    .pipe(_ * countNumTrees(3, 1).toLong) *
    countNumTrees(5, 1).toLong *
    countNumTrees(7, 1).toLong *
    countNumTrees(1, 2).toLong).tap(println)

  def countNumTrees(stepRight: Int, stepDown: Int) = {
    val rows = (0 until numRows).map(_ * stepDown).filter(_ < numRows)
    val cols = rows.zipWithIndex.map {
      case (_, rowIndex) => rowIndex * stepRight
    }

    val wayPoints =
      rows.zip(cols).map { case (row, col) => Coordinates(row, col % numCols) }

    println()

    wayPoints.count(setOfTrees.contains)
  }

  case class Coordinates(row: Int, column: Int)
}
