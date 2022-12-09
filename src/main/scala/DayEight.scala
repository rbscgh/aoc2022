object DayEight {
  private val parse: Seq[String] => Array[(Array[(Int, Int)], Int)] = { lines =>
    lines
      .map(_.split("").map(_.toInt).zipWithIndex.toArray)
      .zipWithIndex.toArray
  }

  def partOne(lines: Seq[String]): Int = {
    val canSeeTreeFromEdge = (x: Seq[Int]) => if (x.contains(1)) 1 else 0
    performTreeOp(parse(lines), isVisibleFrom, canSeeTreeFromEdge).count(_ == 1)
  }


  def partTwo(lines: Seq[String]): Int = {
    val totalScenicScore = (x: Seq[Int]) => x.product
    performTreeOp(parse(lines), calculateScenicScore, totalScenicScore).max
  }

  private def performTreeOp(
              grid: Array[(Array[(Int, Int)], Int)],
              neighborEval: (Array[Int], Int) => Int,
              treeEval: Seq[Int] => Int
  ): Array[Int] = {
    grid.flatMap(trees => {
      val row = trees._2
      trees._1.map(tree => {
        val col = tree._2
        val treeVal = tree._1
        val currentColumn: Array[Int] = grid.map(_._1(col)).map(_._1)
        val currentRow: Array[Int] = trees._1.map(_._1)
        val neighboringTrees: Array[Int] = getNeighborsToVisit(row, col, currentRow, currentColumn)
          .map(direction => neighborEval(direction, treeVal))

        treeEval(neighboringTrees)
      })
    })
  }

  private val isVisibleFrom: (Array[Int], Int) => Int = { (remainingTrees, treeToSee) =>
    if (remainingTrees.isEmpty || (remainingTrees.map(treeToSee > _).toSet == Set(true))) 1
    else 0
  }

  private val calculateScenicScore: (Array[Int], Int) => Int = { (remainingTrees, treeToEvaluate) =>
    remainingTrees.foldLeft((0, true)) { case ((cnt, keepCounting), tree) =>
      (keepCounting, treeToEvaluate > tree) match {
        case (true, true) => (cnt + 1, true)
        case (true, false) => (cnt + 1, false)
        case _ => (cnt, keepCounting)
      }
    }._1
  }

  private def getNeighborsToVisit(row: Int, column: Int, currentRowTrees: Array[Int], currentColumnTrees: Array[Int]): Array[Array[Int]] = Array(
    currentRowTrees.slice(column + 1, currentRowTrees.length),
    currentRowTrees.slice(0, column).reverse,
    currentColumnTrees.slice(0, row).reverse,
    currentColumnTrees.slice(row + 1, currentColumnTrees.length)
  )

  def run(): Unit = {
    val input = io.Source.fromResource("dayeight.txt").getLines().toSeq

    val p1 = partOne(input)
    val p2 = partTwo(input)

    println(s"result for p1: $p1")
    println(s"result for p2: $p2")
  }
}
