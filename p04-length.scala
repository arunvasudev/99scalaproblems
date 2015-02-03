def length(xs: List[Any]): Int = xs match {
    case Nil => 0
    case x :: xs => 1 + length(xs)
}
