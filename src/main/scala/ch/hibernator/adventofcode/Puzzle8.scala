package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.chaining.scalaUtilChainingOps

object Puzzle8 extends App {
  private val source = Source.fromFile("input8.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val instructionsByIndex = input
    .map(rawInstruction => parseInstruction(rawInstruction))
    .zipWithIndex
    .map { case (instruction, index) => (index, instruction) }
    .toMap
  val instructionAccessTimes =
    mutable
      .Map()
      .addAll(instructionsByIndex.keys.toSeq.indices.map((_, 0)).toMap)

  val jmpIndexes = instructionsByIndex
    .filter {
      case (_, instruction) =>
        instruction match {
          case _: Jmp => true
          case _      => false
        }
    }
    .keys
    .toSeq

  val result = jmpIndexes
    .map { jmpIndex =>
      val instrByIndex =
        mutable.Map.newBuilder.addAll(instructionsByIndex).result()
      val toUpdate = instrByIndex(jmpIndex)
      instrByIndex.update(
        jmpIndex,
        Nop(toUpdate.asInstanceOf[Jmp].offset)
      )
      val instrAccessTimes = mutable.Map().addAll(instructionAccessTimes)
      executeInstruction(instrByIndex.toMap, instrAccessTimes, 0, 0)
    }
    .find {
      case (result, _) =>
        result match {
          case Finished     => true
          case InfiniteLoop => false
        }
    }

  println(result)

//  println(executeInstruction(0))
//  println(accumulator)

  sealed abstract class Instruction
  case class Nop(change: Int) extends Instruction
  case class Acc(change: Int) extends Instruction
  case class Jmp(offset: Int) extends Instruction

  sealed trait Result
  case object Finished extends Result
  case object InfiniteLoop extends Result

  @tailrec
  def executeInstruction(
      instrByIndex: Map[Int, Instruction],
      instrAccessTimes: mutable.Map[Int, Int],
      index: Int,
      accumulator: Int
  ): (Result, Int) = {
    if (index >= instrByIndex.size) (Finished, accumulator)
    else {
      val instruction = instrByIndex(index)
      val timesPreviouslyExecuted = instrAccessTimes(index)
      if (timesPreviouslyExecuted > 0) (InfiniteLoop, accumulator)
      else {
        instrAccessTimes.update(
          index,
          instrAccessTimes(index) + 1
        )
        instruction match {
          case _: Nop =>
            executeInstruction(
              instrByIndex,
              instrAccessTimes,
              index + 1,
              accumulator
            )
          case Acc(change) =>
            executeInstruction(
              instrByIndex,
              instrAccessTimes,
              index + 1,
              accumulator + change
            )
          case Jmp(offset) =>
            executeInstruction(
              instrByIndex,
              instrAccessTimes,
              index + offset,
              accumulator
            )
        }
      }
    }
  }

  def parseInstruction(rawInstruction: String): Instruction = {
    val nameAndNumber = rawInstruction.split(" ")
    val name = nameAndNumber.head
    val num = nameAndNumber.last.pipe(parseNumber)
    name match {
      case "nop" => Nop(num)
      case "acc" => Acc(num)
      case "jmp" => Jmp(num)
      case _     => throw new IllegalArgumentException("wrong instruction name")
    }
  }

  def parseNumber(num: String): Int = {
    val sign = num.head
    val number = num.tail.toInt
    if (sign == '-') number * (-1) else number
  }
}
