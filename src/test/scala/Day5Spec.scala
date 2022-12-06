import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class Day5Spec extends AnyFunSpec with should.Matchers {

  val input = """    [D]
                |[N] [C]
                |[Z] [M] [P]
                | 1   2   3
                |
                |move 1 from 2 to 1
                |move 3 from 1 to 3
                |move 2 from 2 to 1
                |move 1 from 1 to 2""".stripMargin



  it("should build stack map") {
    val stacksLines = readFile()._1

    val expected = Map(
      1 -> Seq("Z", "N"),
      2 -> Seq("M", "C", "D"),
      3 -> Seq("P")
    )

    DayFive.buildStackMap(stacksLines) shouldEqual expected
  }


  it("should get the top crates with 9000") {
    DayFive.partOne(readFile()._1, readFile()._2) shouldEqual "CMZ"
  }

  it("should get the top crates with 9001") {
    DayFive.partTwo(readFile()._1, readFile()._2) shouldEqual "MCD"
  }

  def readFile(): (Seq[String], Seq[String]) = {
    val Array(stacks, instructions) = input.split("\n\n")

    (stacks.split("\n"), instructions.split("\n"))
  }
}
