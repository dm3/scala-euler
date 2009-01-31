/*
* For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
* Evaluate the sum of all the amicable numbers under 10000.
*/
class E_21 extends Problem[Int] {
  import Util._
  /**
  * N - the number to find its amicable pair for
  * a - the sum of the proper divisors of N (Ns possible amicable pair)
  * b - if 'a' is Ns amicable pair, b == n
  */
  def sum(n: Int) = {
    val a = sumOfDivisors(n)
    val b = sumOfDivisors(a)
    // a > b condition is needed to remove duplicate (reversed) amicable pairs
    if (a > b && b == n && n != a) Some((n, a, b)) else None
  }

  def result = (1 until 10000).map(sum(_).getOrElse (0, 0, 0)).filter(x => x._2 < 10000).foldLeft(0)((acc, x) => acc + x._1 + x._2)
}//31626

/*
* For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 ? 53 = 49714.
* What is the total of all the name scores in the file?
*/
class E_22 extends Problem[Int] {
  def sum(xs: Seq[Char]) = xs.foldLeft(0)((acc, el) => acc + el.toInt - 64)

  /**
  * Convert each name to a list of integeres (65-90 as there are only uppercase names)
  */
  val names = readLines("data/e_22.txt")(_.split(",").toList.map(_.replaceAll("\"", ""))).next //one line
  override def result = names.filter(_.length > 0).sort(_ < _).zipWithIndex.foldLeft(0)((acc, el) => acc + sum(el._1.toSeq) * (el._2 + 1))
}

class E_23 extends Problem[Int] {
  import Util._
  val upper = 28123 //greatest number which cannot (theoretically) be expressed as the sum of abundant numbers

  /*
   * this condition is satisfying, because the greater the number (x), the greater is the sum of its divisors
   * upper * 6 has been derived empirically :)
   */
  val abundantNums = ints(1).map(x => (x, sumOfDivisors(x))).filter(x => x._1 < x._2).takeWhile(pair => 2 * pair._2 <= upper * 6)
  def result = {
    val abundantSum: mutable.HashSet[Int] = new mutable.HashSet
    for (a <- abundantNums; b <- abundantNums) abundantSum += (a._1 + b._1)
    (1 to upper).filter(!abundantSum.contains(_)).foldLeft(0)(_ + _)
  }
}
