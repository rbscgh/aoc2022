object DaySix {

  def partOne(buffer: Array[Char]): Int = {
    slideAndFind(buffer, 4)
  }

  def partTwo(buffer: Array[Char]): Int = {
    slideAndFind(buffer, 14)
  }

  // Check previous commit for original solution
  // TIL indexWhere -- thanks @maneatingape!!
  private def slideAndFind(buffer: Array[Char], size: Int) = {
    buffer.sliding(size).indexWhere(_.toSet.size == size) + size
  }

  def run(): Unit = {
    val input = io.Source.fromResource("daysix.txt").mkString

    val p1 = partOne(input.toCharArray)
    val p2 = partTwo(input.toCharArray)

    println(s"part one result is: $p1")
    println(s"part two result is: $p2")
  }
}
