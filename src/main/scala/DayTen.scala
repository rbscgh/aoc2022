object DayTen {

  sealed trait Command {
    val blockFor: Int
  }

  case class ADDX(toAdd: Int) extends Command {
    override val blockFor: Int = 2
  }

  case object NOOP extends Command {
    override val blockFor: Int = 1
  }


  private val parseCommand: String => Command = { s =>
    s.split(" ") match {
      case Array("addx", toAdd) => ADDX(toAdd.toInt)
      case _ => NOOP
    }
  }

  val partOne: Seq[String] => Int = { ops =>
    val signalStrengths: Map[Int, Int] = Map(
      20 -> 0,
      60 -> 0,
      100 -> 0,
      140 -> 0,
      180 -> 0,
      220 -> 0
    )

    val (interestingValues, _, _) = ops.foldLeft((signalStrengths, 1, 1)) { case ((strengthMap, cycle, onStack), op) =>
      val cmd = parseCommand(op)
      val (updatedMap, cycleAfterInterrupt) = (1 to cmd.blockFor).foldLeft(strengthMap, cycle) { case ((m, c), _) =>

        val up = if (m.contains(c)) m + (c -> (c * onStack)) else m

        (up, c + 1)
      }

      cmd match {
        case ADDX(toAdd) => (updatedMap, cycleAfterInterrupt, onStack + toAdd)
        case NOOP => (updatedMap, cycleAfterInterrupt, onStack)
      }
    }

    interestingValues.values.sum
  }

  val partTwo: Seq[String] => Unit = { ops =>
    val blankScreen = Seq.empty[String]
    val sprite = (0, 2)

    val (crt, _, _, _) = ops.foldLeft(blankScreen, 1, 1, sprite) { case ((screen, cycle, register, spr), op) =>
      val cmd = parseCommand(op)

      val (updatedScreen, cycleAfterInterrupt) = (1 to cmd.blockFor).foldLeft(screen, cycle) { case ((scr, c), _) =>

        val pixel = decidePixel(c - 1, spr)
        val sc = scr :+ pixel

        (sc, c + 1)
      }

      cmd match {
        case ADDX(toAdd) =>
          val updatedRegister = register + toAdd
          (updatedScreen, cycleAfterInterrupt, updatedRegister, (updatedRegister - 1, updatedRegister + 1))
        case NOOP => (updatedScreen, cycleAfterInterrupt, register, spr)
      }
    }

    crt.grouped(40).map(_.mkString).foreach(println)
  }

  private def decidePixel(c: Int, s: (Int, Int)): String = {
    val level = c / 40
    val padding = level * 40
    if ((s._1 + padding to s._2 + padding).contains(c)) "#" else "."
  }

  def run() = {
    val input = io.Source.fromResource("dayten.txt").getLines().toSeq
    println(partOne(input))
    partTwo(input)
  }
}
