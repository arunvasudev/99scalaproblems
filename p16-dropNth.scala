def dropNth[T](n: Int, xs: List[T]): List[T] = {
    def dropNthImpl[T](n: Int, curr: Int, ys: List[T]): List[T] = ys match {
        case Nil => Nil
        case y :: ys1 => if (curr % n == 0) dropNthImpl(n, (curr + 1) % n, ys1)
                         else y :: dropNthImpl(n, (curr+1)%n, ys1)
    }

    dropNthImpl(n,1,xs)
}
