package ch.hibernator.adventofcode

import scala.io.Source

object Puzzle9 extends App {
  private val source = Source.fromFile("input9.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val allNumbers: Seq[Long] = input.map(_.toLong)
  val numberGroups = allNumbers.sliding(26)

  val firstInvalidNumber = numberGroups
    .find { numbers =>
      val (actualNumber, previousNumbers) = (numbers.last, numbers.init)
      !isNumberValid(actualNumber, previousNumbers)
    }
    .map(_.last)
    .getOrElse(sys.error("No invalid number found"))

  println(firstInvalidNumber)

  var neededSequence: Seq[Long] = Seq()

  allNumbers.tails.toSeq.find { tail =>
    val inits = tail.inits
    inits.exists { init =>
      val isNeededSum = init.size > 1 && init.sum == firstInvalidNumber
      if (isNeededSum) neededSequence = init
      isNeededSum
    }
  }

  println(neededSequence)
  println(neededSequence.min + neededSequence.max)

  def isNumberValid(number: Long, previousNumbers: Seq[Long]): Boolean = {
    (for {
      x <- previousNumbers
      y <- previousNumbers
    } yield x != y && (x + y) == number).contains(true)
  }
}
