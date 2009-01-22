class Problem[T] {
    import scala.compat._
    
    def timedResult(): T = {
        val start = Platform.currentTime
        val res = result()
        val end = Platform.currentTime
        println("duration: " + (end - start).toDouble / 1000 + " sec")
        res
    }
    def result(): T = error(":/")
}

//Find the sum of all the multiples of 3 or 5 below 1000.
class E_1 extends Problem[Int] {
    override def result(): Int = Stream.from(1).take(999)
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

object Main extends Application {
    println(new E_7 timedResult)
}
