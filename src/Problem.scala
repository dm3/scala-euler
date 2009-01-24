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