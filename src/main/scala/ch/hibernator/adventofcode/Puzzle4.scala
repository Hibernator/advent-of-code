package ch.hibernator.adventofcode

import scala.io.Source

object Puzzle4 extends App {
  private val source = Source.fromFile("input4.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()

  val rawPasswports = input
    .foldLeft(Seq("")) {
      case (acc, newLine) =>
        if (newLine.isBlank) acc :+ ""
        else acc.init :+ s"${acc.last} $newLine"
    }
    .map(_.trim)

  val validPassports = rawPasswports.filter { rawPassport =>
    rawPassport.contains("byr:") &&
    rawPassport.contains("iyr:") &&
    rawPassport.contains("eyr:") &&
    rawPassport.contains("hgt:") &&
    rawPassport.contains("hcl:") &&
    rawPassport.contains("ecl:") &&
    rawPassport.contains("pid:")
  }

  val numValidPassports = validPassports.size

  println(numValidPassports)

  val validHairColorCharacter: Set[Char] = Set('0', '1', '2', '3', '4', '5',
    '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
  val validEyeColors: Set[String] =
    Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  val numbers: Set[Char] =
    Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

  val numMoreValidPassports = validPassports.count(isRawPassportValid)

  println(numMoreValidPassports)

  def isRawPassportValid(rawPassport: String): Boolean = {
    rawPassport.split(" ").forall { fieldValue =>
      fieldValue.split(":").toSeq match {
        case Seq("byr", value) =>
          value.toIntOption.exists(year => year >= 1920 && year <= 2002)
        case Seq("iyr", value) =>
          value.toIntOption.exists(year => year >= 2010 && year <= 2020)
        case Seq("eyr", value) =>
          value.toIntOption.exists(year => year >= 2020 && year <= 2030)
        case Seq("hgt", value) =>
          (value.takeRight(2) == "cm" && value.length == 5 && value
            .take(3)
            .toIntOption
            .exists(height => height >= 150 && height <= 193)) ||
            (value.takeRight(2) == "in" && value.length == 4 && value
              .take(2)
              .toIntOption
              .exists(height => height >= 59 && height <= 76))
        case Seq("hcl", value) =>
          value.length == 7 && value.head == '#' && value.tail
            .forall(validHairColorCharacter.contains)
        case Seq("ecl", value) => validEyeColors.contains(value)
        case Seq("pid", value) =>
          value != null && value.length == 9 && value.forall(numbers.contains)
        case Seq(_, _) => true
      }
    }
  }

}
