def penultimate[T](xs: List[T]): Option[T] = xs match {
    case Nil => None
    case x :: Nil => None
    case x :: x1 :: Nil => Some(x)
    case x :: ys => penultimate(ys)
}
