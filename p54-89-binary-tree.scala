package binaryTree

sealed abstract class Tree[+T] {

    // flips a tree about its root node (i.e., takes a mirror image about a line
    // going through the root node
    def flipped: Tree[T]

    def isSymmetric: Boolean

    def addValue[U >: T <% Ordered[U]](v: U): Tree[U]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    def flipped = Node(value, right.flipped, left.flipped)

    def isSymmetric = Tree.areIsomorphic(left, right.flipped)

    def addValue[U >: T <% Ordered[U]](v: U): Tree[U] = 
        if (v == value) this
        else if (v < value) Node(value, left.addValue(v), right)
        else Node(value, left, right.addValue(v))
}

case object End extends Tree[Nothing] {
    override def toString = "."

    def flipped = End

    def isSymmetric = true

    def addValue[U <% Ordered[U]](v: U): Tree[U] = Node(v, Tree.empty[U], Tree.empty[U])
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

    // returns true if t1 and t2 have the same structure
    def areIsomorphic[A, B](t1: Tree[A], t2: Tree[B]): Boolean = (t1, t2) match {
        case (End, End) => true
        case (Node(a1, t1Left, t1Right), Node(b1, t2Left, t2Right)) => 
            areIsomorphic(t1Left, t2Left) && areIsomorphic(t1Right, t2Right)
        case _ => false
    }

    def fromList[T <% Ordered[T]](xs: List[T]): Tree[T] = 
        xs.foldLeft(empty[T])((accum, x) => accum.addValue(x))

    def empty[T]:Tree[T] = End
}
