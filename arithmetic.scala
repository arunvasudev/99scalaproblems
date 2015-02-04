import scala.language.implicitConversions

package arithmetic {
  class S99Int(val num: Int) {
    import S99Int._

    // Problem 31
    // checks whether a given integer is prime
    def isPrime: Boolean = num != 0 && !(2 to Math.sqrt(num).toInt + 1).exists(num % _ == 0)

    // Problem 33
    // checks whether a number is coprime to this number
    def isCoprimeTo(n: Int): Boolean = gcd(num, n) == 1
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    // Problem 32
    // returns the GCD of two given numbers through the Euclidean algorithm
    def gcd(a: Int, b: Int): Int = if (a % b == 0) b else gcd(b, a % b)
  }
}
