def nOf[T](x: T, n: Int): List[T] = if (n <= 0) Nil else x :: nOf(x, n - 1)

def duplicateN[T](xs: List[T], n: Int): List[T] = xs match {
    case Nil => Nil
    case x :: xs1 => nOf(x, n) ::: duplicateN(xs1, n)
}
