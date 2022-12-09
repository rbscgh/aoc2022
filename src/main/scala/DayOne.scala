object DayOne {

  private def combine:Seq[String] => Seq[Int] = { lines =>
    lines
      .map(elf => elf.split("\n").toList.map(_.toInt))
      .map(snacksPerElf => snacksPerElf.sum)
  }

  def partOne: Seq[String] => Int = combine(_).max

  def partTwo: Seq[String] => Int = combine(_).sorted(Ordering.Int.reverse).take(3).sum

  def run(): Unit = {
    val input = io.Source.fromResource("dayone.txt").mkString.split("\n\n").toSeq
    val p1 = partOne(input)
    val p2 = partTwo(input)

    println(s"part one result $p1")
    println(s"part two result $p2")
  }
}
