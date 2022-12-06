import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class Day6Spec extends AnyFunSpec with should.Matchers {


  it("should solve part 1") {
    val input = Seq(
      ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7),
      ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
      ("nppdvjthqldpwncqszvftbrmjlhg", 6),
      ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
      ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11),
    ).foreach(in => {
      val (buffer, expected) = in

      DaySix.partOne(buffer.toCharArray) shouldEqual expected
    })
  }

  it("should solve part 2") {
    val input = Seq(
      ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19),
      ("bvwbjplbgvbhsrlpgdmjqwftvncz", 23),
      ("nppdvjthqldpwncqszvftbrmjlhg", 23),
      ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29),
      ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26),
    ).foreach(in => {
      val (buffer, expected) = in

      DaySix.partTwo(buffer.toCharArray) shouldEqual expected
    })
  }
}
