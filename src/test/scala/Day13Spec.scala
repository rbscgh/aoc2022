class Day13Spec extends AOCSpec {

  it("should solve part 1 "){
    val packetPairs = input.split("\n\n")
    DayThirteen.partOne(packetPairs) shouldEqual 13
  }

  it("should sort") {
    val packetPairs = input.split("\n\n")
    DayThirteen.partTwo(packetPairs) shouldEqual 140
  }


  val input = """[1,1,3,1,1]
                |[1,1,5,1,1]
                |
                |[[1],[2,3,4]]
                |[[1],4]
                |
                |[9]
                |[[8,7,6]]
                |
                |[[4,4],4,4]
                |[[4,4],4,4,4]
                |
                |[7,7,7,7]
                |[7,7,7]
                |
                |[]
                |[3]
                |
                |[[[]]]
                |[[]]
                |
                |[1,[2,[3,[4,[5,6,7]]]],8,9]
                |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin

}
