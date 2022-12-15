import scala.annotation.tailrec

object DayFourteen {

  case class Point(x: Int, y: Int) {
    def +(other: Point) = Point(this.x + other.x, this.y + other.y)
  }

  // leftDown, rightDown, Down
  private val possiblePlaces = Seq(Point(-1, 1), Point(1, 1), Point(0, 1))


  val partOne: Seq[String] => Int = { in =>
    val startingMap: Map[Point, String] = parse(in)
    val maxHeight = startingMap.maxBy(_._1.y)._1.y
    val res = play(startingMap, maxHeight)
    res.count(p => p._2 == "o")
  }

  val partTwo: Seq[String] => Int = { in =>
    val startingMap: Map[Point, String] = parse(in)
    val maxHeight = startingMap.maxBy(_._1.y)._1.y + 2
    val floor = (0 to 1000).map(Point(_, maxHeight) -> "#").toMap
    val mapWithFloor = startingMap ++ floor
    val res = play(mapWithFloor, maxHeight, shouldIncludeTop = true)
    res.count(p => p._2 == "o") + 1 // inclusive :)
  }

  private def parse: Seq[String] => Map[Point, String] = { in =>
    in.flatMap(bl => {
      bl
        .split("->")
        .map(_.trim.split("\\D+").map(_.toInt))
        .map(i => Point(i(0), i(1)))
        .sliding(2).toSeq
        .flatMap(p => blocks(p(0), p(1)))
    }).map(p => p -> "#").toMap
  }


  private def blocks(p1: Point, p2: Point): Seq[Point] = {
    if (p2.x == p1.x) {
      if (p2.y > p1.y) (p1.y to p2.y).map(Point(p1.x, _))
      else (p2.y to p1.y).map(Point(p1.x, _))

    } else {
      if (p2.x > p1.x) (p1.x to p2.x).map(Point(_, p1.y))
      else (p2.x to p1.x).map(Point(_, p1.y))
    }
  }

  @tailrec
  private def play(map: Map[Point, String], maxHeight: Int, shouldIncludeTop: Boolean = false): Map[Point, String] = {

    @tailrec
    def drop(currentPoint: Point): Point = {
      val Seq(leftDown, rightDown, down) = possiblePlaces.map(p => currentPoint + p) // surrounding points

      if ((map.contains(down) && map.contains(leftDown) && map.contains(rightDown))) {
        currentPoint // sand rests
      }
      else if (!shouldIncludeTop && currentPoint.y >= maxHeight) {
        currentPoint // terminate, we're looping
      }
      else if (map.contains(down)) { // check left or right

        if (!map.contains(leftDown)) { // if left open go there
          drop(leftDown)
        }
        else if (!map.contains(rightDown)) { // else right
          drop(rightDown)
        }
        else throw new Exception("should not reach")
      }
      else { // go deeper
        drop(down)
      }
    }

    val entry = Point(500, 0)
    val settled = drop(entry) // where this rested.. could be infinite so cap it by max height

    val ender = if (shouldIncludeTop) settled == entry else settled.y >= maxHeight // terminate
    if (ender) {
      map
    }
    else play(map + (settled -> "o"), maxHeight, shouldIncludeTop = shouldIncludeTop) // add another

  }

  def run() = {
    val input = io.Source.fromResource("dayfourteen.txt").getLines().toSeq

    println(partOne(input))
    println(partTwo(input))

  }
}
