import scala.annotation.tailrec

def checkValue[A, B](l: List[A], value: B, num: Int)(op: A => B): Boolean = {
  @tailrec
  def loop(remaining: List[A], count: Int): Boolean = remaining match {
    case Nil => count == num
    case head :: tail =>
      val newCount = if (op(head) == value) count + 1 else count
      loop(tail, newCount)
  }

  loop(l, 0)
}

@main def zad1: Unit = {
  val l = List(1, 2, 3, 3, 4, 5)
  val value = 0
  val num = 3
  val op: Int => Int = a => a % 2

  val result = checkValue(l, value, num)(op)
  println(s"Result: $result")
}
