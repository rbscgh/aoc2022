object DayEleven {

  case class Monkee(items: Seq[BigInt], operation: BigInt => BigInt, test: BigInt => Boolean, t: Int, f: Int, inspections: BigInt = 0)

  val partOne: Seq[String] => BigInt = { in =>
    doTheMonkeyWithMe(in, makesMeLessWorried = true, 20)
  }

  val partTwo: Seq[String] => BigInt = { in =>
    doTheMonkeyWithMe(in, makesMeLessWorried = false, 10_000)
  }

  private def doTheMonkeyWithMe(rawMonkey: Seq[String], makesMeLessWorried: Boolean, rounds: Int): BigInt = {
//    val mod = rawMonkey.map(block => block.split("\n")(3).split("\\D+").last.toInt).product
    val init: Map[Int, Monkee] = parse(rawMonkey)
//      .map(m => m.copy(operation = m.operation.apply(_) % mod))
      .zipWithIndex
      .map(m => m._2 -> m._1)
      .toMap


    val end = (0 until rounds).foldLeft(init) { (monkees, rnd) =>

      val update = (0 until monkees.size).foldLeft(monkees) { (monks, index) =>
        val currentMonk = monks(index)

        if (currentMonk.items.nonEmpty) {
          val playWorries = currentMonk.items.map(y => currentMonk.operation.apply(y))
          val postBoredWorries: Seq[BigInt] = playWorries.map(w => if (makesMeLessWorried) w / 3 else w)

          val throwResult = postBoredWorries.map { i =>
            if (currentMonk.test(i)) (currentMonk.t, i) else (currentMonk.f, i)
          }

          val afterMnk = throwResult.foldLeft(monks) { (map, res) =>
            map + (res._1 -> map(res._1).copy(items = map(res._1).items :+ res._2))
          }

          val updatedInspection = currentMonk.inspections + currentMonk.items.size

          afterMnk + (index -> afterMnk(index).copy(items = List.empty, inspections = updatedInspection))
        } else {
          monks
        }
      }

      update
    }

    end.map(_._2.inspections).toList.sorted.reverse.take(2).product
  }

  private val getOpFunc: String => BigInt => BigInt = { s =>
    val scalar = s.split("\\+ | \\*").last.replaceAll(" ", "")
    if (s.contains("+")) {
      BigInt(scalar) + _
    } else {
      if (scalar.equals("old")) {
        x: BigInt => BigInt(Math.pow(x.toDouble, 2.0).toInt)
      } else {
        BigInt(scalar) * _
      }
    }
  }

  private def isDiv(x: BigInt, div: BigInt): Boolean = x % div == 0
  private def divBy(div: BigInt): BigInt => Boolean = isDiv(_, div)

  private val getTestFunc: String => BigInt => Boolean = { rule =>
    val numerator = rule.split("\\D+").last.toInt
    divBy(numerator)
  }


  def parse: Seq[String] => Seq[Monkee] = { s =>
    s.map(block => {
      val infos = block.split("\n")
      val operation = infos(2).split("=")(1)
      val test = infos(3)
      val t = infos(4).split("\\D+").last.toInt
      val f = infos(5).split("\\D+").last.toInt
      Monkee(
        infos(1).split("\\D+").filterNot(_.isEmpty).map(_.replaceAll(" ", "").toInt).map(BigInt(_)),
        getOpFunc(operation),
        getTestFunc(test), t, f)
    })
  }

  def run() = {
    val input = io.Source.fromResource("dayeleven.txt").mkString.split("\n\n").toSeq
    println(partOne(input))
    println(partTwo(input))
  }
}
