import scala.collection.immutable.HashSet

object DayNine {

  sealed trait Direction {
    val effect: Point
  }

  case object Right extends Direction {
    val effect = Point(1, 0)
  }
  case object Left extends Direction {
    override val effect: Point = Point(-1, 0)
  }
  case object Up extends Direction {
    override val effect: Point = Point(0, 1)
  }
  case object Down extends Direction {
    override val effect: Point = Point(0, -1)
  }
  case class Diag(X: Direction, Y: Direction) extends Direction {
    override val effect: Point = Point(X.effect.x, Y.effect.y)
  }

  case class Point(x: Int, y: Int) {
    def +(p: Point): Point = Point(p.x + x, p.y + y)
  }
  case class Movement(direction: Direction, steps: Int)

  private val toMovements: String => Movement = { in =>
    val split = in.split(" ")
    val step = split(1).toInt
    split(0) match {
      case "R" => Movement(Right, step)
      case "L" => Movement(Left, step)
      case "U" => Movement(Up, step)
      case "D" => Movement(Down, step)
    }
  }

  val partOne: Seq[String] => Int = { input =>
    val parsedMoves = input.map(toMovements)
    val startingRope = Seq.fill[Point](2)(Point(0, 0))
    moveRope(parsedMoves, startingRope)
  }

  val partTwo: Seq[String] => Int = { input =>
    val parsedMoves: Seq[Movement] = input.map(toMovements)
    val startingRope = Seq.fill[Point](10)(Point(0, 0))
    moveRope(parsedMoves, startingRope)
  }

  private def moveRope(moves: Seq[Movement], startingRope: Seq[Point]): Int = {
    val init = HashSet(Point(0, 0))

    val (_, visited) = moves.foldLeft((startingRope, init)) { case ((rope, tailvists), movement) =>
      (1 to movement.steps).foldLeft(rope, tailvists) { case ((r, tv), _) =>

        val updatedHead = r.head + movement.direction.effect
        val updatedRope = r.tail.scanLeft(updatedHead) { (h, t) =>
          val distance = getDistance(h, t)
          if (distance >= 2.0) t + getDirectionToMove(h,t).effect else t
        }

        (updatedRope, tv + updatedRope.last)
      }
    }

    visited.size
  }

  private def getDistance(a: Point, b: Point): Double = math.hypot(a.x - b.x, a.y - b.y)

  private def getDirectionToMove(head: Point, tail: Point): Direction = {
    if (head.x == tail.x) {
      if (head.y - tail.y > 0) Up else Down
    }
    else if (head.y == tail.y) {
      if (head.x - tail.x > 0) Right else Left
    } else {
      val X = if (head.x - tail.x > 0) Right else Left
      val Y = if (head.y - tail.y > 0) Up else Down
      Diag(X, Y)
    }
  }

  def run(): Unit = {
    val input = io.Source.fromResource("daynine.txt").getLines().toSeq

    val p1 = partOne(input)
    val p2 = partTwo(input)

    println(s"the result of p1: $p1")
    println(s"the result of p1: $p2")
  }
}
