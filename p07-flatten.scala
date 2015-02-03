def flatten(xs: List[Any]): List[Any] = xs match {
    case Nil => Nil
    case x :: xs1 =>  {x match {
                case x1:List[_] => flatten(x1)
                case _ => List(x)
            }} ::: flatten(xs1)
}
