import scala.collection.immutable.HashSet

object DaySixteen {

  case class ValveDetails(rate: Int, nextValves: Seq[String])

  def visitedPartOne: Seq[String] => Int = { lines =>
    val (valves, start) = parse(lines)

    (1 to 30).foldLeft((HashSet.empty[String], Set.empty[String], start, "", 0)) {
      case ((openedValves, visited, currentValve, lastValue, eventualPressure), min) =>
        println(s"Min: $min -- visiting $currentValve current open valves: $openedValves")
        val deets = valves(currentValve)
        if (deets.rate == 0 || openedValves.contains(currentValve)) {
          // must move
          val nextNode = moveTo2(deets, visited, lastValue)
          (openedValves, visited + currentValve, nextNode, currentValve, eventualPressure)
        }
        else {
          // check if there are viable nodes 1 away that have higher potential and go there
          // get this nodes potential
          val currentPotentialRate = deets.rate * min

          val hasBiggerFish = deets.nextValves.exists({ valv =>
            !openedValves.contains(valv) && valves(valv).rate * (min - 1) > currentPotentialRate
          })

          if (hasBiggerFish) {
            // dont turn this valve yet
            val nextNode = moveTo2(deets, visited, lastValue)
            (openedValves, visited + currentValve, nextNode, currentValve, eventualPressure)
          } else {
            val pressure = deets.rate * min
            (openedValves + currentValve, visited + currentValve, currentValve, lastValue, eventualPressure + pressure)
          }
        }
    }._5
  }

  def dfs(node: String, vmap: Map[String, ValveDetails], openedValves: Seq[String], visited: Set[String] = Set.empty[String], pressure: Int = 0, minute: Int = 1): Int = {

    if (minute == 30) {
      pressure
    }
    else {
      val updatedVisited = visited + node

      if (!openedValves.contains(node)) {

        if (!hasBiggerFish(node, vmap, openedValves, minute)) {
          // turn
          val deets = vmap(node)
          val currentPotentialRate = deets.rate * minute
          dfs(vmap(node).nextValves.head, vmap, updatedVisited, pressure + currentPotentialRate, )
        }
      }
    }

  }

  def hasBiggerFish(node: String, vmap: Map[String, ValveDetails], openedValves: Seq[String], min: Int) = {
    val deets = vmap(node)
    val currentPotentialRate = deets.rate * min

    deets.nextValves.exists({ valv =>
      !openedValves.contains(valv) && vmap(valv).rate * (min - 1) > currentPotentialRate
    })
  }


  def moveTo2(deets: ValveDetails, visited: Set[String], lastValue: String): String = {
    val nodeToChoose = deets.nextValves
      .filter(s => s == lastValue)
      .map(visited.contains)
      .indexWhere(_ == false)

    if (deets.nextValves.size == 1) {
      deets.nextValves.head
    } else if (nodeToChoose == -1) { // all neighbors have been visited
      // now find the one which itself has a unvisted neighbor
      deets.nextValves.head
    } else {
      deets.nextValves(nodeToChoose)
    }
  }

  //  val partOne: Seq[String] => Int = { lines =>
  //    val (vmap, start) = parse(lines)
  //
  //    (1 to 30).foldLeft((vmap, HashSet(start), start, "", 0)) {
  //      case ((valves, openedValves, currentValve, lastValve, eventualPressure), min) =>
  //        println(s"Min: $min -- visiting $currentValve current open valves: $openedValves")
  //        val deets = valves(currentValve)
  //
  //        if (openedValves.contains(currentValve)) {
  //          // have to move
  //          val nextValve = moveTo(deets, lastValve)
  //          val updatedValveTrail = getUpdatedTrail(deets.nextValves)
  //          val updatedVMap = valves.updated(currentValve, deets.copy(nextValves = updatedValveTrail))
  //
  //          (updatedVMap, openedValves, nextValve, currentValve, eventualPressure)
  //        } else if () {
  //
  //        }
  //        else {
  //          // check if there are viable nodes 1 away that have higher potential and go there
  //          // get this nodes potential
  //          val currentPotentialRate = deets.rate * min
  //
  //          val hasBiggerFish = deets.nextValves.exists({ valv =>
  //            !openedValves.contains(valv) && valves(valv).rate * (min - 1) > currentPotentialRate
  //          })
  //
  //          if (hasBiggerFish) {
  //            // we shouldn't open the valve here and target the next
  //            val nextValve = moveTo(deets, lastValve)
  //            val updatedValveTrail = getUpdatedTrail(deets.nextValves)
  //            val updatedVMap = valves.updated(currentValve, deets.copy(nextValves = updatedValveTrail))
  //
  //            (updatedVMap, openedValves + currentValve, nextValve, currentValve, eventualPressure)
  //          } else {
  //            // open
  //            val pressure = deets.rate * min
  //            (valves, openedValves + currentValve, currentValve, lastValve, eventualPressure + pressure)
  //          }
  //
  //        }
  //    }._5
  //  }

  def getUpdatedTrail(valves: Seq[String]): Seq[String] = {
    if (valves.size == 1) valves else valves.tail :+ valves.head
  }

  def moveTo(deets: ValveDetails, lastValve: String): String = {
    if (deets.nextValves.size == 1) {
      deets.nextValves.head
    }
    else if (deets.nextValves.head == lastValve) {
      // take the other one
      deets.nextValves.tail.head
    } else {
      deets.nextValves.head
    }
  }


  private val parse: Seq[String] => (Map[String, ValveDetails], String) = { lines =>
    import scala.util.matching.Regex.Match.unapply
    val regex = "[A-Z]{2}".r
    val st = regex.findAllMatchIn(lines.head).toSeq

    (lines.map { l =>
      val valves = regex.findAllMatchIn(l).toSeq
      val rate = l.split("\\D+").filter(_.nonEmpty).head.toInt
      val currentValve = unapply(valves.head).get
      val nextValves = valves.tail.flatMap(unapply)
      currentValve -> ValveDetails(rate, nextValves)
    }.toMap, unapply(st.head).get)
  }

}
