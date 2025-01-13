import scala.annotation.tailrec

def checkValue[A, B](l: List[A], value: B, num: Int)(op: A => B): Boolean = {
  @tailrec
  def loop(remaining: List[A], count: Int): Int = remaining match {
    case Nil => count
    case head :: tail =>
      if (op(head) == value) loop(tail, count + 1)
      else loop(tail, count)
  }

  loop(l, 0) == num
}

@main def zad1: Unit = {
  val l = List(1, 2, 3, 3, 4, 5)
  val value = 0
  val num = 3
  val op = (a: Int) => a % 2

  println(checkValue(l, value, num)(op))
}