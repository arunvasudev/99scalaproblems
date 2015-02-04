import scala.language.implicitConversions

package arithmetic {
  class S99Int(val num: Int) {
    import S99Int._

    // Problem 31
    // checks whether a given integer is prime
    def isPrime: Boolean = num != 0 && !(2 to Math.sqrt(num).toInt).exists(num % _ == 0)

    // Problem 33
    // checks whether a number is coprime to this number
    def isCoprimeTo(n: Int): Boolean = gcd(num, n) == 1

    // Problem 34
    // Euler's totient function - the number of integers between 1 and num that are coprime to 
    // this number
    def totient: Int = (1 to num).count(isCoprimeTo(_:Int))

    // Problem 35
    // Returns the list of prime factors of this number
    def primeFactors: List[Int] = {
        import scala.collection.mutable
        val buf = new mutable.ListBuffer[Int]
        for(p <- (2 to num).filter(_.isPrime)){
            var n = num
            while (n % p == 0){
                n /= p
                buf += p
            }
        }

        buf.toList
    }

    // Problem 36
    // Returns the prime factors with their multiplicity
    def primeFactorsMultiplicity: Map[Int, Int] = {
        import scala.collection.mutable
        val map = new mutable.HashMap[Int, Int]
        val factors = primeFactors

        def runlengthEncode[T](xs: List[T]): List[(T, Int)] = xs match {
            case Nil => Nil
            case (y::ys) => (y, xs.takeWhile(_ == y).length) :: runlengthEncode(xs.dropWhile(_ == y))
        }

        map ++= runlengthEncode(factors)
        map.toMap
    }

    // Problem 37
    // Improved totient function using the multiplicative formula
    def totientImproved: Int = 
        primeFactorsMultiplicity.foldLeft(1)((acc, pair) => acc * Math.pow(pair._1, pair._2 - 1).toInt * (pair._1 - 1))

    // Problem 40
    // Goldbach's conjecture: returns two primes that sum to this number
    def goldbach: (Int, Int) = {
        for(n1 <- 2 to (num/2 +3).toInt) if (n1.isPrime && (num - n1).isPrime) return (n1, num - n1)
        return (0, 0)
    }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    // Problem 32
    // returns the GCD of two given numbers through the Euclidean algorithm
    def gcd(a: Int, b: Int): Int = if (a % b == 0) b else gcd(b, a % b)

    // Problem 39
    // Return a list of prime numbers in a given range
    def listPrimesInRange(xs: Seq[Int]): Seq[Int] = xs.filter(_.isPrime)

    // Problem 41 - A
    // Given a range of numbers, returns the goldbach composition of all even integers between them
    def goldbachList(xs: Seq[Int]): Map[Int, (Int, Int)] = { 
        import scala.collection.immutable.TreeMap
        xs.filter(_ % 2 == 0).foldRight(new TreeMap[Int, (Int, Int)])((n, map) => map + (n -> n.goldbach))
    }

    // Problem 41 - B
    // Given a range of numbers, returns the goldbach composition of all even integers between them
    // whose primes are both greater than a given limit
    def goldbachListLimited(xs: Seq[Int], lim: Int): Map[Int, (Int, Int)] = {
        val origGBs = goldbachList(xs)
        origGBs.filter({case (n, (p1, p2)) => p1 > lim && p2 > lim})
    }
  }
}
