import scala.language.implicitConversions

package S99Logic {

class S99Boolean(val v: Boolean) {
    
    import S99Boolean._

    def and(b2: Boolean): Boolean = v && b2
    def or(b2: Boolean): Boolean = v || b2
    def xor(b2: Boolean) = !(v == b2)
}

object S99Boolean {
    implicit def BooleanToS99(b: Boolean): S99Boolean = {
        return new S99Boolean(b)
    }

    def not(b: Boolean): Boolean = !b

    def table2(func:(Boolean, Boolean) => Boolean) {
        println("A     B     result")
        for{a <- List(true, false)
            b <- List(true, false)} {
            println(f"$a%-5s $b%-5s ${func(a,b)}%-5s")
        }
    }
}
}
