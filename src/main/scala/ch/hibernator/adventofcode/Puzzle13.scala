package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Puzzle13 extends App {
  private val source = Source.fromFile("input13.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val startTime = input.head.toLong
  val buses = input.last.split(",").filterNot(_ == "x").map(_.toLong).toSeq
  println(startTime)
  println(buses)

  val waitingTimesByBus: Map[Long, Long] = buses.map { bus =>
    (bus, earliestBusDeparture(bus) - startTime)
  }.toMap

  val result = waitingTimesByBus.map { case (bus, waitingTime) =>
    bus * waitingTime
  }.min

  println(result)

  def earliestBusDeparture(bus: Long): Long = {
    val previousDeparture = startTime - (startTime % bus)
    previousDeparture + bus
  }

  //Second part
  val delaysByBuses = input.last.split(",").toSeq
    .zipWithIndex.filterNot { case (bus, _) => bus == "x" }
    .map { case (bus, delay) => (bus.toLong, delay.toLong) }
  println(delaysByBuses)
//  val firstValidTimestamp = findFirstValidTimestamp(100000000000000L)
//  println(firstValidTimestamp)

  @tailrec
  def findFirstValidTimestamp(currentTimestamp: Long): Long = {
    println(currentTimestamp)
    if (isValidTimestamp(currentTimestamp)) currentTimestamp
    else findFirstValidTimestamp(currentTimestamp + 863L)
  }

  def isValidTimestamp(time863: Long): Boolean = {
    val actualTimestamp = time863 - 23
    delaysByBuses.forall { case (bus, delay) =>
      (actualTimestamp + delay) % bus == 0
    }
  }
}
