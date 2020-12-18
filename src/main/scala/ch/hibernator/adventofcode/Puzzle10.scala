package ch.hibernator.adventofcode

import org.apache.commons.lang3.StringUtils

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Puzzle10 extends App {
  private val source = Source.fromFile("input10.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val sortedAdapters = input.map(_.toInt).sorted
  val allJolts = 0 +: sortedAdapters :+ (sortedAdapters.last + 3)

  val differences = allJolts.init.zip(allJolts.tail).map {
    case (first, second) =>
      second - first
  }

  val result = differences.count(_ == 1) * differences.count(_ == 3)
  println(result)

  val allJolts2 = 0 +: sortedAdapters :+ (sortedAdapters.last + 3)
  val differences2 = allJolts2.init.zip(allJolts2.tail).map {
    case (first, second) =>
      second - first
  }

  println(allJolts2.map(_.toString).map(StringUtils.leftPad(_, 3)))
  println(" " +: differences2.map(_.toString.pipe(StringUtils.leftPad(_, 3))))

  val sequences = allJolts2.tail
    .foldLeft(Seq[Seq[Int]](Seq(0))) {
      case (acc, nextNumber) =>
        if (nextNumber == acc.last.last + 1)
          acc.init :+ (acc.last :+ nextNumber)
        else acc :+ Seq(nextNumber)
    }
    .map(_.size)
    .filter(_ > 2)
    .map {
      case 3 => 2
      case 4 => 4
      case 5 => 7
    }
    .map(BigDecimal.apply)
    .product

  println(sequences)

//  println(174 / 3)

//  val numCombinations = (55 to 103).foldLeft(0L) {
//    case (soFarValid, numAdapters) =>
//      println(s"started $numAdapters")
//      val validCombinations =
//        sortedAdapters.combinations(numAdapters).count(isValid).toLong
//      println(s"finished $numAdapters")
//      soFarValid + validCombinations
//  }
//
//  println(numCombinations)

  def isValid(sequence: Seq[Int]): Boolean = {
    val fullSequence = -2 +: sequence :+ 172
    val diffs = fullSequence.init.zip(fullSequence.tail).map {
      case (first, second) =>
        second - first
    }
    diffs.forall(_ <= 3)
  }

  println(isValid(sortedAdapters))
  println(allJolts2.size)
  println(sortedAdapters.combinations(103).size)
}
