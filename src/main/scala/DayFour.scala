object DayFour {

  def partOne(pairings: Seq[String]): Int =
    expandPairs(pairings).count((pairing: Array[Set[Int]]) => {
      pairing match {
        case Array(elf1, elf2) => (elf1.subsetOf(elf2) || elf2.subsetOf(elf1))
      }
    })

  def partTwo(pairings: Seq[String]): Int = expandPairs(pairings).count { (pairing: Array[Set[Int]]) => {
    pairing match {
      case Array(elf1, elf2) => elf1.intersect(elf2).nonEmpty
    }
  }}

  private def expandPairs(pairings: Seq[String]): Seq[Array[Set[Int]]] = {
    pairings.map(_.split(",").map(_.split("-").map(_.toInt)).map {
      case Array(lower, upper) => (lower to upper).toSet
    })
  }


  def run(): Unit = {
    val input = io.Source.fromResource("dayfour.txt").getLines().toSeq
    val p1 = partOne(input)
    val p2 = partTwo(input)

    println(s"result of p1: $p1")
    println(s"result of p2: $p2")
  }
}
