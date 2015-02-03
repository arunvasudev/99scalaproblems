def insertAt[T](x: T, i: Int, xs: List[T]): List[T] = xs.take(i) ::: List(x) ::: xs.drop(i)
