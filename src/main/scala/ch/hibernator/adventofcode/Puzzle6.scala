package ch.hibernator.adventofcode

import scala.io.Source

object Puzzle6 extends App {

  private val source = Source.fromFile("input6.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val anyoneYesCount = input
    .foldLeft(Seq("")) {
      case (acc, newLine) =>
        if (newLine.isBlank) acc :+ ""
        else acc.init :+ s"${acc.last} $newLine"
    }
    .map(_.trim)
    .map(_.replace(" ", ""))
    .map(_.distinct)
    .map(_.length)
    .sum

  val groups = input
    .foldLeft(Seq(Seq[String]())) {
      case (acc, newLine) =>
        if (newLine.isBlank) acc :+ Seq[String]()
        else acc.init :+ (acc.last :+ newLine)
    }

  val everyoneYesCount =
    groups.map(_.map(_.toSet)).map(intersectAll).map(_.size).sum

  def intersectAll(sets: Seq[Set[Char]]): Set[Char] = {
    sets.tail.foldLeft(sets.head) {
      case (intermediateIntersection, nextSet) =>
        intermediateIntersection.intersect(nextSet)
    }
  }

  println(everyoneYesCount)

}
