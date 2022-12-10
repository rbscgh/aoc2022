class D9Spec extends AOCSpec {

  val input = """R 4
                |U 4
                |L 3
                |D 1
                |R 4
                |D 1
                |L 5
                |R 2""".stripMargin




  it("should count number of visited") {
    DayNine.partOne(input.split("\n")) shouldEqual 13
  }

  it("should count number of visited with 10 knots") {
    DayNine.partTwo(input.split("\n")) shouldEqual 1
  }

  it("should count visited 10 with larger set") {
    val i = """R 5
              |U 8
              |L 8
              |D 3
              |R 17
              |D 10
              |L 25
              |U 20""".stripMargin

    DayNine.partTwo(i.split("\n")) shouldEqual 36
  }

}
