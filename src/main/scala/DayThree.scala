object DayThree extends App {

  private lazy val prioMap =
    ('a' to 'z').zipWithIndex.map(r => (r._1, r._2 + 1)).toMap ++
    ('A' to 'Z').zipWithIndex.map(r => (r._1, r._2 + 27)).toMap

  def partOne(rucksacks: Seq[String]): Int = {

    rucksacks.map(sack => {
      sack.splitAt(sack.length / 2) match {
        case (first, second) => prioMap(
          first.toSet
            .intersect(second.toSet)
            .head
        )
      }
    }).sum
  }

  def partTwo(rucksack: Seq[String]): Int = {
    val groups: Seq[Seq[String]] = rucksack.sliding(3, 3).toSeq

    groups.map {
      case Seq(first, second, third) => prioMap(
        first.toSet
          .intersect(second.toSet)
          .intersect(third.toSet)
          .head
      )
    }.sum
  }

  def run(): Unit = {
    val input = io.Source.fromResource("daythree.txt").getLines().toSeq
    val p1 = partOne(input)
    val p2 = partTwo(input)

    println(s"part one result: $p1")
    println(s"part two result: $p2")
  }
}
