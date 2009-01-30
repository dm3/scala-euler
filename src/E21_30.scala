class E_21 extends Problem[Int] {
  def divisors(n: Int): List[Int] = {
    /**
    * loop gets all the proper divisors of N, including N itself
    * there might be duplicated divisors (and there probably will be)
    * as there is no check for those divisors during recursion
    */
    def loop(n: Int): List[Int] = (2 until n/2).flatMap(x => {
        if (n % x == 0) x :: n :: loop(n/x) else Nil
      }).toList
    1 :: loop(n) match {
      case Nil => Nil
      /**
      * throw the N away
      */
      case xs => xs.removeDuplicates.sort(_ < _).init
    }
  }

  /**
  * N - the number to find its amicable pair for
  * a - the sum of the proper divisors of N (Ns possible amicable pair)
  * b - if 'a' is Ns amicable pair, b == n
  */
  def sum(n: Int) = {
    val a = divisors(n).foldLeft(0)(_ + _)
    val b = divisors(a).foldLeft(0)(_ + _)
    // a > b condition is needed to remove duplicate (reversed) amicable pairs
    if (a > b && b == n && n != a) Some((n, a, b)) else None
  }

  override def result = (1 until 7000).map(sum(_).getOrElse (0, 0, 0)).filter(x => x._2 < 10000).foldLeft(0)((acc, x) => acc + x._1 + x._2)
}//31626
