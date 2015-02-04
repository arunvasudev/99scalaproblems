import scala.language.implicitConversions

package arithmetic {
  class S99Int(val num: Int) {
    import S99Int._

    // Problem 31
    def isPrime: Boolean = !(2 to Math.sqrt(num).toInt + 1).exists(num % _ == 0)
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  }
}
