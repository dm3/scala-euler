class E_21 extends Problem[Int] {
  def sumOfDiv(n: Int) = (2 to Math.sqrt(n)).map(x => if (n % x == 0) x + n / x else 0).foldLeft(1)(_ + _)

  /**
  * N - the number to find its amicable pair for
  * a - the sum of the proper divisors of N (Ns possible amicable pair)
  * b - if 'a' is Ns amicable pair, b == n
  */
  def sum(n: Int) = {
    val a = sumOfDiv(n)
    val b = sumOfDiv(a)
    // a > b condition is needed to remove duplicate (reversed) amicable pairs
    if (a > b && b == n && n != a) Some((n, a, b)) else None
  }

  def result = (1 until 10000).map(sum(_).getOrElse (0, 0, 0)).filter(x => x._2 < 10000).foldLeft(0)((acc, x) => acc + x._1 + x._2)
}//31626
