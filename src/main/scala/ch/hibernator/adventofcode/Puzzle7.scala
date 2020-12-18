package ch.hibernator.adventofcode

import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Puzzle7 extends App {
  private val source = Source.fromFile("input7.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  //  val allBagsByColor: mutable.Map[String, Bag] = mutable.Map()
  //  allIntermediateBagsByColor.values.foreach { intermediateBag =>
  //    val bag = allBagsByColor.getOrElse(intermediateBag.color, Bag(intermediateBag.color, Map()))
  //  }

  case class IntermediateBag(
      color: String,
      innerBags: Map[String, Int]
  ) {
    val empty: Boolean = innerBags.isEmpty
  }

  def parseRawBagToIntermediate(rawBag: String): IntermediateBag = {
    val words = rawBag.split(" ")
    val color = words.take(2).mkString(" ")
    val innerBags =
      if (words.mkString(" ").contains("no other bag")) Map[String, Int]()
      else
        words
          .drop(4)
          .grouped(4)
          .map { rawInnerBag =>
            (rawInnerBag.slice(1, 3).mkString(" "), rawInnerBag.head.toInt)
          }
          .toMap
    IntermediateBag(color, innerBags)
  }

  val allIntermediateBagsByColor = input
    .map(parseRawBagToIntermediate)
    .map { bag =>
      (bag.color, bag)
    }
    .toMap

  def bagContainsColor(bag: IntermediateBag, color: String): Boolean = {
    if (bag.empty) false
    else if (bag.innerBags.keys.toSet.contains(color)) true
    else
      bag.innerBags.exists {
        case (innerBagColor, _) =>
          allIntermediateBagsByColor(innerBagColor).pipe(
            bagContainsColor(_, color)
          )
      }
  }

  def numBagsContainingColor(color: String): Int =
    allIntermediateBagsByColor.values.count(bagContainsColor(_, color))

  def numBagsInside(color: String): Int = {
    val bag = allIntermediateBagsByColor(color)
    if (bag.empty) 0
    else
      bag.innerBags.values.sum + bag.innerBags.keys.map { innerColor =>
        numBagsInside(innerColor) * bag.innerBags(innerColor)
      }.sum
  }

  println(numBagsContainingColor("shiny gold"))
  println(numBagsInside("shiny gold"))

}
