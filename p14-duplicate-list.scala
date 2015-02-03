def duplicateList[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case x :: xs1 => x :: x :: duplicateList(xs1)
}
