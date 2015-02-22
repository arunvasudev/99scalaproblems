package mtree

case class MTree[+T](value: T, children: List[MTree[T]]) {
    override def toString = 
        if (!children.isEmpty) 
            "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
        else
            "M(" + value.toString + ")"
}

object MTree {
    def apply[T](value: T):MTree[T] = MTree(value, List())
}
