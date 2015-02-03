def runlengthDirectImpl[T](xs: List[T], x: T, c: Int, ps: List[(T, Int)]): List[(T, Int)] = xs match {
    case Nil => ((x, c) :: ps).reverse 
    case x1 :: xs1 => if (x == x1) runlengthDirectImpl(xs1, x, c + 1, ps)
                      else runlengthDirectImpl(xs1, x1, 1, (x, c) :: ps)
}

def runlengthDirect[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil => Nil
    case x :: xs1 => runlengthDirectImpl(xs1, x, 1, Nil)
}
