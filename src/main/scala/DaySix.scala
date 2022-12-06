import com.sun.xml.internal.fastinfoset.util.CharArray

object DaySix {

  def partOne(buffer: Array[Char]): Int = {
    slideAndFind(buffer, 4)
  }

  def partTwo(buffer: Array[Char]): Int = {
    slideAndFind(buffer, 14)
  }

  private def slideAndFind(buffer: Array[Char], slideBy: Int) = {
    buffer
      .sliding(slideBy)
      .zipWithIndex
      .find(_._1.toSet.size == slideBy)
      .map(slideBy + _._2)
      .get
  }

  def run(): Unit = {
    val input = io.Source.fromResource("daysix.txt").mkString

    val p1 = partOne(input.toCharArray)
    val p2 = partTwo(input.toCharArray)

    println(s"part one result is: $p1")
    println(s"part two result is: $p2")
  }
}
