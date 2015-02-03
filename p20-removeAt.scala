def removeAt[T](i: Int, xs: List[T]): (List[T], T) = (xs.take(i) ::: xs.drop(i + 1), xs.drop(i).head)
