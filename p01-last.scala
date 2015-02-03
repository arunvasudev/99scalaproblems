def last[T](xs: List[T]): Option[T] = xs match {
    case Nil => None
    case x :: Nil => Some(x)
    case x :: xs => last(xs)
}
