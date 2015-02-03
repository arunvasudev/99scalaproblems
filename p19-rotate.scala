def rotate[T](n: Int, xs: List[T]): List[T] = {
    val len = xs.length
    if (n == 0) xs
    else if (n > 0) {
        val n1 = n % len
        xs.drop(n1) ::: xs.take(n1)
    }
    else {
        val n1 = len - (n.abs % len)
        xs.drop(n1) ::: xs.take(n1)
    }
}
