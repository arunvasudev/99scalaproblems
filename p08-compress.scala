def compressImpl[T](first: T, rest: List[T]): List[T] = rest match {
    case Nil => List(first)
    case x :: xs => if (x == first) compressImpl(first, xs) else first :: compressImpl(x, xs)
}

def compress[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case _ => compressImpl(xs.head, xs.tail)
}
