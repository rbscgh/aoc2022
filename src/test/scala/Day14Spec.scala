class Day14Spec extends AOCSpec {


  it("should simulate sand 🏜") {
    DayFourteen.partOne(input.split("\n")) shouldEqual 21
  }

  it("should solve part 2") {
    DayFourteen.partTwo(input.split("\n")) shouldEqual 93
  }

  val input =
    """498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

}
