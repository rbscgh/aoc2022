import scala.io.Source

object DayOne {

  val readFile: List[String] = {
    val source = Source.fromFile("src/main/input/dayone.txt")
    val input = source.mkString.split("\n\n").toList
    source.close()
    input
  }

  private def combineAndSort: List[Int] = {
    readFile
      .map(elf => elf.split("\n").toList.map(_.toInt))
      .map(snacksPerElf => snacksPerElf.sum)
      .sorted(Ordering.Int.reverse)
  }

  def partOne: Int = {
    combineAndSort.head
  }

  def partTwo: Int = {
    combineAndSort.slice(0, 3).sum
  }

  def run(): Unit = {
    val p1 = partOne
    val p2 = partTwo

    println(s"part one result $p1")
    println(s"part two result $p2")
  }
}
