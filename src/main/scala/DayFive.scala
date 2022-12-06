

object DayFive {

  def buildStackMap(stackLines: Seq[String]): Map[Int, Seq[String]] = {
    val emptyMap = Map.empty[Int, Seq[String]]

    // stack col -> stack number
    val columnToStack = stackLines
      .reverse
      .take(1)
      .map(_.split(""))
      .flatMap(_.zipWithIndex.filterNot(_._1.equals(" ")))
      .map(r => r._2 -> r._1.toInt).toMap

    val inputZippedToColumn = stackLines.dropRight(1)
      .map(_.split(""))
      .map(_.zipWithIndex
        .filterNot(_._1.equals(" ")) // spaces
        .filterNot(r => r._1.equals("[") || r._1.equals("]"))) // braces

    inputZippedToColumn.foldLeft(emptyMap) { (mapSoFar, row) =>
      row.foldLeft(mapSoFar) { (upmap, token) =>
        val col = token._2
        val crate = token._1

        val stack: Int = columnToStack(col)
        upmap + (stack -> (upmap.get(stack).map(Seq(crate) ++ _).getOrElse(Seq(crate))))
      }
    }
  }

  def partOne(stacks: Seq[String], instructions: Seq[String]): String = {
    val startingMap = buildStackMap(stacks)

    instructions.foldLeft(startingMap) { (mapSoFar, instruction) =>

      val Array(howMany, fromStack, toStack) = instruction.split("\\D+").filter(_.nonEmpty).map(_.toInt)
      val numberOfMoves = 1 to howMany

      numberOfMoves.foldLeft(mapSoFar) { (intermediateMap, _) =>
        val crateToMove: String = intermediateMap(fromStack).last
        val updatedFromStack = intermediateMap(fromStack).dropRight(1)
        val updatedToStack = intermediateMap(toStack) ++ Seq(crateToMove)
        val updatedFrom = intermediateMap + (fromStack -> updatedFromStack)

        updatedFrom + (toStack -> updatedToStack)
      }
    }.toList.sortBy(_._1).map(_._2.last).mkString
  }

  def partTwo(stacks: Seq[String], instructions: Seq[String]): String = {
    val startingMap = buildStackMap(stacks)

    instructions.foldLeft(startingMap){ (mapSoFar, instruction) =>
      val Array(howMany, fromStack, toStack) = instruction.split("\\D+").filter(_.nonEmpty).map(_.toInt)

      val cratesToMove: Seq[String] = mapSoFar(fromStack).takeRight(howMany)
      val updatedFromStack = mapSoFar(fromStack).dropRight(howMany)
      val updatedToStack = mapSoFar(toStack) ++ cratesToMove
      val afterFrom = mapSoFar + (fromStack -> updatedFromStack)
      val afterTo = afterFrom + (toStack -> updatedToStack)

      afterTo
    }
  }.toList.sortBy(_._1).map(_._2.last).mkString


  def run(): Unit = {
    val Array(stacks, instructions) = io.Source.fromResource("dayfive.txt").mkString.split("\n\n")

    val p1 = partOne(stacks.split("\n"), instructions.split("\n"))
    val p2 = partTwo(stacks.split("\n"), instructions.split("\n"))

    println(s"result part 1: $p1")
    println(s"result part 2: $p2")
  }
}
