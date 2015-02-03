def nof[T](x: T, n: Int): List[T] = if (n <= 0) Nil else x :: nof(x, n - 1)


def decodeRunlength[T](ps: List[(T, Int)]): List[T] = ps match {
    case Nil => Nil
    case p :: ps1 => nof(p._1, p._2) ::: decodeRunlength(ps1)
}
