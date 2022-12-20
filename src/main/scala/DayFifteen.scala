object DayFifteen {

  case class Point(x: Int, y: Int) {
    def +(other: Point) = Point(this.x + other.x, this.y + other.y)

    def getDistance(other: Point): Int = (this.x - other.x).abs + (this.y - other.y).abs
  }

  sealed trait Device

  case object Sensor extends Device

  case object Beacon extends Device

  case object Nothing extends Device // this is dumb maybe dont need

  def partOne(lines: Seq[String], col: Int): Int = {
    val grid = parse(lines)

    val numberOfNonEmpty = grid.count { case (coord, thing) => coord.y == col && thing == Nothing }

    numberOfNonEmpty
  }

  private val parse: Seq[String] => Map[Point, Device] = { lines =>
    val regex = "(-?\\d+)".r
    val nums = lines
      .map(regex.findAllIn)
      .map(_.toSeq)

    nums.foldLeft(Map.empty[Point, Device]) { (map, reading) =>
      val Seq(sx, sy, bx, by) = reading.map(_.toInt)

      val sensor = Point(sx, sy)
      val beacon = Point(bx, by)
      val distance = sensor.getDistance(beacon) // now we need to extend RL this much

      //      val bounds = Seq(Point(distance * -1, 0), Point(distance, 0), Point(0, distance), Point(0, distance * -1)).map(sensor + _)
      val Seq(left, right) = Seq(Point(distance * -1, 0), Point(distance, 0)).map(sensor + _)

      val updatedMap = map
        .updated(sensor, Sensor)
        .updated(beacon, Beacon)


      val (afterFill, _) = (left.x to right.x).foldLeft((updatedMap, 0)) { case ((mapSoFar, amtToFill), x) =>
        // update left to right

        val upper = sensor.y + amtToFill
        val lower = sensor.y - amtToFill
        val partialPointMap = (lower to upper)
          .map(p => Point(x, p))
          .filterNot(mapSoFar.contains)
          .map(_ -> Nothing)
          .toMap
          .updated(Point(x, sensor.y), Nothing)

        if (x >= sensor.x) {
          (mapSoFar ++ partialPointMap, amtToFill - 1)
        } else {
          (mapSoFar ++ partialPointMap, amtToFill + 1) // left to center
        }
      }

      afterFill
    }
  }

  def run(): Unit = {
    val input = io.Source.fromResource("dayfifteen.txt").getLines().toSeq

    println(partOne(input, 2_000_000))

  }
}
