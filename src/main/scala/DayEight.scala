object DayEight {

  private val buildGrid: Seq[String] => Array[(Array[(Int, Int)], Int)] = { lines =>
    lines
      .map(_.split("").map(_.toInt).zipWithIndex.toArray)
      .zipWithIndex.toArray
  }

  def partOne(lines: Seq[String]): Int = {
    val grid = buildGrid(lines)

    grid.flatMap(trees => {
      val row = trees._2
      trees._1.map(tree => {
        val col = tree._2
        val treeVal = tree._1
        val verticalTrees: Array[Int] = grid.map(_._1(col)).map(_._1)
        val (right, left, up, down) = getNeighborsToVisit(row, col, trees._1, verticalTrees)

        val isVisibleFromRight  = isVisibleFrom(right, treeVal)
        val isVisibleFromLeft   = isVisibleFrom(left, treeVal)
        val isVisibleFromUp     = isVisibleFrom(up, treeVal)
        val isVisibleFromDown   = isVisibleFrom(down, treeVal)


        Seq(isVisibleFromUp, isVisibleFromDown, isVisibleFromLeft, isVisibleFromRight).contains(true)
      })
    }).count(_ == true)
  }

  private def isVisibleFrom(remainingTrees: Array[Int], treeToSee: Int): Boolean = {
    remainingTrees.isEmpty || (remainingTrees.map(treeToSee > _).toSet == Set(true))
  }

  def partTwo(lines: Seq[String]): Int = {
    val grid = buildGrid(lines)

    grid.flatMap(trees => {
      val row = trees._2
      trees._1.map(tree => {
        val col = tree._2
        val treeVal = tree._1
        val verticalTrees = grid.map(_._1(col)).map(_._1)

        val (right, left, up, down) = getNeighborsToVisit(row, col, trees._1, verticalTrees)

        val treeScoreRight  = calculateScenicScore(right, treeVal)
        val treeScoreLeft   = calculateScenicScore(left, treeVal)
        val treeScoreUp     = calculateScenicScore(up, treeVal)
        val treeScoreDown   = calculateScenicScore(down, treeVal)

        treeScoreRight * treeScoreLeft * treeScoreDown * treeScoreUp
      })
    }).max
  }

  private def calculateScenicScore(remainingTrees: Array[Int], treeToEvaluate: Int): Int = {
    remainingTrees.foldLeft((0, true)) { case ((cnt, keepCounting), tree) =>
      (keepCounting, treeToEvaluate > tree) match {
        case (true, true) => (cnt + 1, keepCounting)
        case (true, false) => (cnt + 1, false)
        case (false, true) | (false, false) => (cnt, keepCounting)
      }
    }._1
  }

  private def getNeighborsToVisit(row: Int, column: Int, currentRowTrees: Array[(Int, Int)], verticalTrees: Array[Int]):
  (Array[Int], Array[Int], Array[Int], Array[Int]) = {
    (
      currentRowTrees.slice(column + 1, currentRowTrees.length).map(_._1),
      currentRowTrees.slice(0, column).reverse.map(_._1),
      verticalTrees.slice(0, row).reverse,
      verticalTrees.slice(row + 1, verticalTrees.length)
    )
  }

  def run(): Unit = {
    val input = io.Source.fromResource("dayeight.txt").getLines().toSeq

    val p1 = partOne(input)
    val p2 = partTwo(input)

    println(s"result for p1: $p1")
    println(s"result for p2: $p2")
  }
}
