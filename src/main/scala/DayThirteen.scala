import scala.annotation.tailrec

object DayThirteen {

  val partOne: Seq[String] => Int = { pRaw =>
    pRaw
      .zipWithIndex
      .filter(pp => haveRightOrder(pp._1))
      .map(_._2 + 1)
      .sum
  }

  private val haveRightOrder: String => Boolean = { ppRaw =>
    ppRaw.split("\n") match {
      case Array(left, right) =>
        eval(left.replaceAll("10", ":"), right.replaceAll("10", ":"))
    }
  }

  val partTwo: Seq[String] => Int = { pRaw =>
    val allpackets = pRaw.flatMap(_.split("\n")).toList
    val replaced = allpackets.map(_.replaceAll("10", ":"))
    val withDividers = replaced :+ "[[2]]" :+ "[[6]]"

    val res = bubblesort(withDividers).reverse

    Seq(
      (res.indexWhere(s => s == "[[2]]") + 1),
      (res.indexWhere(s => s == "[[6]]") + 1)
    ).product
  }

  def run() = {
    val input = io.Source.fromResource("daythirteen.txt").mkString.split("\n\n").toSeq

    println(partOne(input))
    println(partTwo(input))
  }

  private def bubblesort(source: Seq[String]): Seq[String] = {

    def sort(iteration: Seq[String], source: Seq[String], result: Seq[String]): Seq[String] = source match {
      case h1 :: h2 :: rest =>
        if (eval(h1, h2)) sort(iteration, h1 :: rest, result :+ h2) else sort(iteration, h2 :: rest, result :+ h1)
      case l :: Nil =>
        sort(iteration, Nil, result :+ l)
      case Nil =>
        if (iteration.isEmpty) result else sort(iteration.dropRight(1), result, Nil)
    }

    sort(source, source, Nil)
  }

  @tailrec
  private def eval(left: String, right: String): Boolean = {

    if (left.nonEmpty && right.nonEmpty) {
      if (left.head != right.head) { // if not the same chars
        (left.head, right.head) match { // we need to either resolve or continue on
          case (']', _) => true // because the right is bigger
          case (_, ']') => false // because the right is too small
          case ('[', num) =>
            val wrapped = '[' + num.toString + ']'
            eval(left, wrapped + right.tail)
          case (num, '[') =>
            val wrapped = '[' + num.toString + ']'
            eval(wrapped + left.tail, right)
          case (l, r) => r > l // should be two different numbers
          case _ => throw new Exception("should not reach")
        }
      }
      else eval(left.tail, right.tail) // cut head and move on
    }
    else throw new Exception("should not reach")
  }

}
