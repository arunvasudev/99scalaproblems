package binaryTree

sealed abstract class Tree[+T]

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}
case object End extends Tree[Nothing] {
    override def toString = "."
}

object Node {
    def apply[T](value: T):Node[T] = Node(value, End, End)
}

object Tree {
    // Problem 55
    // Construct completely balanced binary trees 
    def cBalanced[T](n: Int, x: T): List[Tree[T]] = 
        if (n <= 0) 
            List(empty[T])
        else {
            if ((n - 1) % 2 == 0){
                val subs = cBalanced((n - 1)/2, x)
                for{ s1 <- subs 
                     s2 <- subs } yield(Node(x, s1, s2))
            }
            else {
                val l1 = for{ s1 <- cBalanced((n - 1)/2, x)
                              s2 <- cBalanced((n - 1)/2 + 1, x) } yield(List(Node(x, s1, s2), Node(x, s2, s1)))
                l1.foldLeft(List.empty[Tree[T]])((accum, l) => l ::: accum)
            }
        }

    def empty[T]:Tree[T] = End
}
