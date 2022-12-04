import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class Day4Spec extends AnyFunSpec with should.Matchers {

  private val input: String =
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8""".stripMargin

  trait Context {
    val lines: Seq[String] = input.split("\n").toSeq
  }

  it("should count redundancy") {
    new Context {
      DayFour.partOne(lines) shouldEqual 2
    }
  }

  it("should count overlaps") {
    new Context {
      DayFour.partTwo(lines) shouldEqual 4
    }
  }

}
