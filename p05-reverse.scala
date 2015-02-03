def reverse[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case x :: xs1 => reverse(xs1) ::: List(x)
}

def efficientReverse[T](xs: List[T]): List[T] = {
    var result = List[T]()
    var rem = xs
    while(!rem.isEmpty){
        result = rem.head :: result
        rem = rem.tail
    }

    result
}

def efficientRecursiveReverseImpl[T](res: List[T], rem: List[T]): List[T] = rem match {
    case Nil => res
    case x :: xs1 => efficientRecursiveReverseImpl(x :: res, rem.tail)
}

def efficientRecursiveReverse[T](xs: List[T]): List[T] = 
    efficientRecursiveReverseImpl(List[T](), xs)
