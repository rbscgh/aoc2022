import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class Day7Spec extends AnyFunSpec with should.Matchers {


  val input =
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k""".stripMargin



  it("solves part one") {
    val terminalOutput = input.split("\n")

    DaySeven.partOne(terminalOutput) shouldEqual 95437
  }

  it("solves part two") {
    val terminalOutput = input.split("\n")

    DaySeven.partTwo(terminalOutput) shouldEqual 24933642
  }
}
