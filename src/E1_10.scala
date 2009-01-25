//Find the sum of all the multiples of 3 or 5 below 1000.
class E_1 extends Problem[Int] {
    override def result = Stream.from(1).take(999)
            .filter(el => el % 3 == 0 || el % 5 == 0)
            .foldLeft(0)(_ + _)
} //233168

//Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million.
class E_2 extends Problem[Int] {
    lazy val fib: Stream[Int] = Stream.cons(0,
        Stream.cons(1, fib.zip(fib tail).map(p => p._1 + p._2)))

    override def result = fib.filter(_ % 2 == 0).takeWhile(_ < 4000000).foldLeft(0)(_ + _)
} //4613732

//What is the largest prime factor of the number 600851475143 ?
class E_3 extends Problem[Long] {
    def calc(n: Long): Long = {
        val p = Stream.from(2).filter(n % _ == 0).head
        if (p == n) p else calc(n / p)
    }

    override def result = calc(600851475143L)
} //6857

class E_4 extends Problem[Int] {
    def isPalindrome(chars: String): Boolean = chars.reverse.mkString == chars

    override def result = (for (a <- (100 until 1000);
                                      b <- (a until 1000);
                                      val res = a * b if isPalindrome(res toString))
                                  yield res).toList.sort(_ > _).head
} //906609

//What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?
class E_5 extends Problem[Int] {
    override def result = (1 to 20).foldLeft(1) { (acc, n) =>
        val r_raw = acc % n
        val r = if (r_raw == 0) n else r_raw
        acc * (if (n % r == 0) n / r else n)
    }
} //232792560

//Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
class E_6 extends Problem[BigInt] {
    import scala.BigInt._

    lazy val sumOfSquares: BigInt = (1 to 100).map(x => x * x).foldLeft(0)(_ + _)
    lazy val squareOfSum: BigInt = (1 to 100).foldLeft(0)(_ + _).pow(2)
    override def result = squareOfSum - sumOfSquares
} //25164150

//What is the 10001^(st) prime number?
class E_7 extends Problem[BigInt] {
    import scala.BigInt._

    override def result = Stream.from(1)
            .filter(_ isProbablePrime 100).zipWithIndex
            .takeWhile(_._2 <= 10000).last._1
} //104743

//Find the greatest product of five consecutive digits in the 1000-digit number.
class E_8 extends Problem[Int] {
    def read(digits: List[Int]): List[Int] = digits match {
        case Nil => Nil
        case _ => digits.take(5).foldLeft(1)(_ * _) :: read(digits drop 1)
    }

    override def result = read((
            "73167176531330624919225119674426574742355349194934" +
            "96983520312774506326239578318016984801869478851843" +
            "85861560789112949495459501737958331952853208805511" +
            "12540698747158523863050715693290963295227443043557" +
            "66896648950445244523161731856403098711121722383113" +
            "62229893423380308135336276614282806444486645238749" +
            "30358907296290491560440772390713810515859307960866" +
            "70172427121883998797908792274921901699720888093776" +
            "65727333001053367881220235421809751254540594752243" +
            "52584907711670556013604839586446706324415722155397" +
            "53697817977846174064955149290862569321978468622482" +
            "83972241375657056057490261407972968652414535100474" +
            "82166370484403199890008895243450658541227588666881" +
            "16427171479924442928230863465674813919123162824586" +
            "17866458359124566529476545682848912883142607690042" +
            "24219022671055626321111109370544217506941658960408" +
            "07198403850962455444362981230987879927244284909188" +
            "84580156166097919133875499200524063689912560717606" +
            "05886116467109405077541002256983155200055935729725" +
            "71636269561882670428252483600823257530420752963450")
            .toList.map(_.toString.toInt)).sort(_ > _).head    //implicit conversion doesn't work :/
} //40824

//Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.
class E_9 extends Problem[Int] {
    lazy val triplet: (Int, Int, Int) = (for (a <- (1 to 1000);
                                             b <- (a to 1000);
                                            val res = (a, b, 1000 - (a + b)))
                                         yield res)
            .filter(x => x._1 * x._1 + x._2 * x._2 == x._3 * x._3)
            .first

    override def result = triplet._1 * triplet._2 * triplet._3
} //31875000

//Find the sum of all the primes below two million.
class E_10 extends Problem[Long] {
    import scala.BigInt._

    val sum: Long = (2 until 2000000).filter(_ isProbablePrime 2000).foldLeft(0L)(_ + _)

    override def result = sum
} //142913828922