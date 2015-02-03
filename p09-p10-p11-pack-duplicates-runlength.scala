def packDuplicatesImpl[T](xs: List[T], x: T, headList: List[T], res: List[List[T]]): List[List[T]] = xs match {
    case Nil => (headList :: res).reverse
    case x1 :: xs1 => if (x1 == x) packDuplicatesImpl(xs1, x, x :: headList, res)
                      else packDuplicatesImpl(xs1, x1, List(x1), headList :: res)
}

def packDuplicates[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => packDuplicatesImpl(xs1, x, List(x), Nil)
}

def runLengthEncode[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil => Nil
    case _ => packDuplicates(xs).map((xs: List[T]) => (xs.head, xs.length)) 
}

def modifiedRunLengthEncode[T](xs: List[T]): List[Any] = xs match {
    case Nil => Nil
    case _ => runLengthEncode(xs).map( (p: (T, Int)) => if (p._2 == 1) p._1 else p)
}
