

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
      .map(_.zipWithIndex.filter(_._1.matches("\\w")))
      .flatMap(_.toList)

    inputZippedToColumn.foldLeft(emptyMap) { (mapSoFar, token) =>
      val col = token._2
      val crate = token._1

      val stack: Int = columnToStack(col)
      mapSoFar + (stack -> (mapSoFar.get(stack).map(Seq(crate) ++ _).getOrElse(Seq(crate))))
    }
  }

  private def performCraneMovements(stacks: Seq[String], instructions: Seq[String], maintainOrder: Boolean): String = {
    val startingMap = buildStackMap(stacks)

    instructions.foldLeft(startingMap) { (mapSoFar, instruction) =>
      val Array(howMany, fromStack, toStack) = instruction.split("\\D+").filter(_.nonEmpty).map(_.toInt)

      val cratesToMove: Seq[String] = mapSoFar(fromStack).takeRight(howMany)
      val updatedFromStack = mapSoFar(fromStack).dropRight(howMany)
      val updatedToStack = mapSoFar(toStack) ++ (if (maintainOrder) cratesToMove else cratesToMove.reverse)
      val afterFrom = mapSoFar + (fromStack -> updatedFromStack)
      val afterTo = afterFrom + (toStack -> updatedToStack)

      afterTo
    }
  }.toList.sortBy(_._1).map(_._2.last).mkString


  def partOne(stacks: Seq[String], instructions: Seq[String]): String =
    performCraneMovements(stacks, instructions, maintainOrder = true)

  def partTwo(stacks: Seq[String], instructions: Seq[String]): String =
    performCraneMovements(stacks, instructions, maintainOrder = false)


  def run(): Unit = {
    val Array(stacks, instructions) = io.Source.fromResource("dayfive.txt").mkString.split("\n\n")

    val p1 = partOne(stacks.split("\n"), instructions.split("\n"))
    val p2 = partTwo(stacks.split("\n"), instructions.split("\n"))

    println(s"result part 1: $p1")
    println(s"result part 2: $p2")
  }
}
