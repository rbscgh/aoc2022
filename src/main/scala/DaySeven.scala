import scala.annotation.tailrec

object DaySeven {

  def partOne(lines: Seq[String]): Int = {
    parseDirSizes(lines).values.filter(s => s < 100000).sum
  }

  def partTwo(lines: Seq[String]): Int = {
    val totalSpace = 70000000
    val desiredUnusedSpace = 30000000

    val dirSizes = parseDirSizes(lines)
    val totalCurrentSize = dirSizes("home")

    val freeSpace = totalSpace - totalCurrentSize

    dirSizes
      .values
      .toList
      .sorted
      .find(size => size + freeSpace >= desiredUnusedSpace)
      .get
  }

  private val parseDirSizes: Seq[String] => Map[String, Int] = { terminalOutput =>
    val emptyPath = List.empty[String]
    val emptyMap = Map.empty[String, Int]
    val emptyAssociatedDirs = Map.empty[String, Seq[String]]

    val (_, pathToSize, associatedDirs) = terminalOutput.foldLeft((emptyPath, emptyMap, emptyAssociatedDirs)) {
      case ((currentPath, dirFileSizeMap, associatedDirs), output) =>

        output.split("\\s") match {
          case Array("$", "cd", "/") => (currentPath :+ "home", dirFileSizeMap, associatedDirs)
          case Array("$", "cd", "..") => (currentPath.dropRight(1), dirFileSizeMap, associatedDirs)
          case Array("$", "cd", name) => (currentPath :+ name, dirFileSizeMap, associatedDirs)
          case Array("dir", name) =>
            val path = currentPath.mkString("/")
            val pathOfChildDir = path + "/" + name
            val updatedChildDirs = associatedDirs + (path -> associatedDirs.get(path).map(_ ++ Seq(pathOfChildDir)).getOrElse(Seq(pathOfChildDir)))
            (currentPath, dirFileSizeMap, updatedChildDirs)
          case Array("$", "ls") => (currentPath, dirFileSizeMap, associatedDirs)
          case Array(size, _) =>
            val path = currentPath.mkString("/")
            val fileSize = size.toInt
            val updatedMap = dirFileSizeMap + (path -> dirFileSizeMap.get(path).map(_ + fileSize).getOrElse(fileSize))
            (currentPath, updatedMap, associatedDirs)
        }
    }

    // account for empty dirs
    val allDirs = pathToSize.keys.toSet.union(associatedDirs.keys.toSet)

    allDirs.foldLeft(pathToSize) { (mapSoFar, dir) =>
      val dirs = associatedDirs.getOrElse(dir, Seq())
      val childPaths = getAllChildPaths(associatedDirs, dirs, dirs)
      val selfSize = pathToSize.getOrElse(dir, 0)
      val sum = childPaths.map(p => pathToSize.getOrElse(p, 0)).sum
      mapSoFar + (dir -> (sum + selfSize))
    }
  }

  @tailrec
  private def getAllChildPaths(associatedDirs: Map[String, Seq[String]], currentInnerDirs: Seq[String], pathsToLookUp: Seq[String]): Seq[String] = {
    if (currentInnerDirs.isEmpty) {
      pathsToLookUp
    }
    else {
      val res = currentInnerDirs.flatMap(p => associatedDirs.getOrElse(p, Seq()))
      getAllChildPaths(associatedDirs, res, pathsToLookUp ++ res)
    }
  }

  def run() = {
    val input = io.Source.fromResource("dayseven.txt").getLines().toSeq
    val p1 = partOne(input)
    val p2 = partTwo(input)

    println(s"result for p1: $p1")
    println(s"result for p2: $p2")
  }
}
