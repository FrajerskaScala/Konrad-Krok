def merge[A](a: List[A], b: List[A])(leq: (A, A) => Boolean): List[A] = {
  @annotation.tailrec
  def loop(a: List[A], b: List[A], acc: List[A]): List[A] = (a, b) match {
    case (Nil, Nil) => acc.reverse
    case (Nil, _)   => (b.reverse ++ acc).reverse
    case (_, Nil)   => (a.reverse ++ acc).reverse
    case (ah :: at, bh :: bt) =>
      if (leq(ah, bh)) loop(at, b, ah :: acc)
      else loop(a, bt, bh :: acc)
  }

  loop(a, b, Nil)
}
