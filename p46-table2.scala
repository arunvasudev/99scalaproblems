def and(b1: Boolean, b2: Boolean): Boolean = b1 && b2
def or(b1: Boolean, b2: Boolean): Boolean = b1 || b2
def not(b: Boolean): Boolean = !b
def xor(b1: Boolean, b2: Boolean) = !(b1 == b2)

def table2(func:(Boolean, Boolean) => Boolean) {
    println("A     B     result")
    for{a <- List(true, false)
        b <- List(true, false)} {
        println(f"$a%-5s $b%-5s ${func(a,b)}%-5s")
    }
}
