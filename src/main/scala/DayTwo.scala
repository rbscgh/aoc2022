import scala.io.Source

object DayTwo {
  val readFile: Seq[String] = {
    val source = Source.fromFile("src/main/input/daytwo.txt")
    val input = source.mkString.split("\n").toSeq
    source.close()
    input
  }

  def run() = {
    val p1 = partOne()
    val p2 = partTwo()

    println(s"part one is: $p1")
    println(s"part two is: $p2")
  }


  def partOne(): Int = getRounds.map(rnd => decodeAndScore(rnd)).sum

  private val decodeAndScore: Seq[String] => Int = { rnd =>
    rnd.map(decode) match {
      case Seq(op, me) => scoreRound(op, me)
      case _ => throw new Exception("WTF")
    }
  }

  def partTwo(): Int = getRounds
    .map(rnd => formDecision(rnd))
    .map({ case (op, me) => scoreRound(op, me) })
    .sum

  private val formDecision: Seq[String] => (Move, Move) = { round =>
    val getLosingMove: Move => Move = {
      case Rock => Scissors
      case Paper => Rock
      case Scissors => Paper
    }

    val getWinningMove: Move => Move = {
      case Rock => Paper
      case Paper => Scissors
      case Scissors => Rock
    }

    round match {
      case Seq(op, shouldDo) =>
        val decodedOpMove = decode(op)
        val myMove = shouldDo match {
          case "X" => getLosingMove(decodedOpMove) // should lose
          case "Y" => decodedOpMove // should draw
          case "Z" => getWinningMove(decodedOpMove) // should win
        }

        (decodedOpMove, myMove)
    }
  }


  // shared

  sealed trait Move {
    def points: Int
  }

  case object Rock extends Move {
    override def points: Int = 1
  }

  case object Paper extends Move {
    override def points: Int = 2
  }

  case object Scissors extends Move {
    override def points: Int = 3
  }

  private def scoreRound(opponent: Move, me: Move): Int = {
    val bonusPoints = (opponent, me) match {
      case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => 6 // win
      case (Paper, Rock) | (Scissors, Paper) | (Rock, Scissors) => 0 // loss
      case _ => 3 // tie
    }

    bonusPoints + me.points
  }

  private val decode: String => Move = { move =>
    if (List("A", "X").contains(move)) Rock
    else if (List("B", "Y").contains(move)) Paper
    else if (List("C", "Z").contains(move)) Scissors
    else throw new Exception("WTF decode")
  }

  private val getRounds = {
    readFile.map(_.split(" ").toSeq)
  }
}
