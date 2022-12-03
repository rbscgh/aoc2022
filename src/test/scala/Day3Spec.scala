import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class Day3Spec extends AnyFunSpec with should.Matchers {

  val input =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw
      |""".stripMargin

  it("should count sacks") {
    val l = input.split("\n").toSeq
    DayThree.partOne(l) shouldEqual 157
  }

  it("should find unique between group of three") {
    val l = input.split("\n").toSeq
    DayThree.partTwo(l) shouldEqual 70
  }
}
