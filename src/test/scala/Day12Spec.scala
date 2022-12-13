class Day12Spec extends AOCSpec {

  val input =
    """Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi""".stripMargin


  it("should") {
    DayTwelve.partOne(input.split("\n")) shouldEqual 31
  }

  it("bfds") {
    DayTwelve.partTwo(input.split("\n")) shouldEqual 29
  }

}
