import scala.collection.immutable

object DayTwelve {
  case class Point(x: Int, y: Int) {
    def +(point: Point) = Point(point.x + this.x, point.y + this.y)
  }

  private val offsets = Array(Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))

  val partOne: Seq[String] => Int = input => BFS(input, 'S')

  val partTwo: Seq[String] => Int = input => BFS(input, 'a')

  def isEdge(point: Point, width: Int, height: Int): Boolean =

    point.y == 0 || point.x == 0 || (point.x == 0 && point.y == height - 1) || (point.x == width - 1 && point.y == 0)

  private def BFS(input: Seq[String], start: Char) = {
    val grid = (for {
      x <- input.head.indices
      y <- input.indices
    } yield Point(x, y) -> input(y)(x)).toMap

    val entries: Map[Point, Char] = grid.filter(sp => sp._2 == start && isEdge(sp._1, input.head.length, input.length))
    val desiredPosition = grid.map(_.swap)('E')

    entries.map(e => {
      val initQ = List(e._1)
      val distMap = Map(e._1 -> 0)

      val (_, res) = grid.foldLeft((initQ, distMap)) { case ((queue, dist), _) =>

        if (queue.nonEmpty) {
          val popped = queue.head
          val updatedQueue = queue.drop(1)

          val neighbors = offsets.map(_ + popped).filter(
            p => p.x >= 0
              && p.y >= 0
              && p.y < input.length
              && p.x < input.head.length
              && elevation(grid(p)) - elevation(grid(popped)) <= 1
              && !dist.contains(p)
          )

          val updatedDistances = neighbors.foldLeft(dist) { (m, n) =>
            val distance: Int = m(popped) + 1
            m + (n -> distance)
          }

          (updatedQueue ++ neighbors, updatedDistances)
        }
        else {
          (queue, dist)
        }
      }

      res.getOrElse(desiredPosition, Int.MaxValue)
    }).toSeq.min

  }

  private val elevation: Char => Char = { pv =>
    if ('E' == pv) 'z'
    else if ('S' == pv) 'a'
    else pv
  }

  def run() = {
    val input = io.Source.fromResource("daytwelve.txt").getLines().toSeq

    println(partOne(input))
    println(partTwo(input))
  }
}
