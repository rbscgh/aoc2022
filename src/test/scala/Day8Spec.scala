class Day8Spec extends AOCSpec {


  val input =
    """30373
      |25512
      |65332
      |33549
      |35390""".stripMargin


  it("should count visable trees") {
    val i = parse
    DayEight.partOne(i) shouldEqual 21
  }

  it("should find most scenic tree") {
    val i = parse
    DayEight.partTwo(i) shouldEqual 8
  }

  val parse: Seq[String] = {
    input.split("\n").toSeq
  }
}
