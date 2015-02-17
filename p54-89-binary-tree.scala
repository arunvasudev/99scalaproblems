package binaryTree

sealed abstract class Tree[+T] {

    // flips a tree about its root node (i.e., takes a mirror image about a line
    // going through the root node)
    def flipped: Tree[T]

    def isSymmetric: Boolean

    def addValue[U >: T <% Ordered[U]](v: U): Tree[U]

    def nodeCount: Int

    def leafCount: Int

    def leafList: List[T]

    def internalList: List[T]

    def atLevel(n: Int): List[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    def flipped = Node(value, right.flipped, left.flipped)

    def isSymmetric = Tree.areIsomorphic(left, right.flipped)

    def addValue[U >: T <% Ordered[U]](v: U): Tree[U] = 
        if (v == value) this
        else if (v < value) Node(value, left.addValue(v), right)
        else Node(value, left, right.addValue(v))

    def nodeCount = 1 + left.nodeCount + right.nodeCount

    def leafCount = this match {
        case Node(v, End, End) => 1
        case _ => left.leafCount + right.leafCount
    }

    def leafList = this match {
        case Node(v, End, End) => List(v)
        case _ => left.leafList ++ right.leafList
    }

    def internalList = this match {
        case Node(v, l, r) if (l != Tree.empty[T] || r != Tree.empty[T]) => v::(l.internalList ++ r.internalList)
        case _ => List()
    }

    def atLevel(n: Int) = 
        if (n == 1) List(value)
        else left.atLevel(n - 1) ++ right.atLevel(n - 1)
}

case object End extends Tree[Nothing] {
    override def toString = "."

    def flipped = End

    def isSymmetric = true

    def addValue[U <% Ordered[U]](v: U): Tree[U] = Node(v, Tree.empty[U], Tree.empty[U])

    def nodeCount = 0

    def leafCount = 0

    def leafList = List()

    def internalList = List()

    def atLevel(n: Int) = List()
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

    // problem 58
    // Generate symmetric balanced trees through generate and test
    def symmetricBalancedTrees[T](n: Int, v: T): List[Tree[T]] = 
        cBalanced(n, v).filter(_.isSymmetric)

    // problem 59
    // construct a list of height balanced trees of height h
    def hBalancedTrees[T](h: Int, v: T): List[Tree[T]] = 
        if (h < 0) List()
        else
        if (h == 0) List(empty[T])
        else {
            val h1Set = hBalancedTrees(h - 1, v)
            val h2Set = hBalancedTrees(h - 2, v)

            import scala.collection.mutable
            val buf = new mutable.ListBuffer[Tree[T]]
            for{t1 <- h1Set; t2 <- h1Set} buf += Node(v, t1, t2)

            for{t1 <- h1Set; t2 <- h2Set} {
                buf += Node(v, t1, t2)
                buf += Node(v, t2, t1)
            }

            buf.toList
        }

    // given a height h, what's the minimum number of nodes a height balanced
    // tree can have? Implementation of the formula: Min(h) = 1 + Min(h - 1) + Min(h - 2)
    // generates an infinite series of (height, minNodeCount) pairs
    def minHBalancedNodeCounts: Stream[(Int, Int)] = {
        def helper(a: Int, b: Int, h: Int): Stream[(Int, Int)] = 
            (h, a) #:: helper(b, a+b+1, h+1)

        helper(0, 1, 0)
    }

    def minHBalancedNodes(h: Int): Int = {
        if (h <= 0) return 0
        val (h1, n1) = minHBalancedNodeCounts.drop(h).head
        n1
    }

    def maxHBalancedNodes(h: Int): Int = {
        math.ceil(math.pow(2, h)).toInt - 1
    }

    // given a node-count n, what's is the maximum height that a height balanced tree with 
    // n nodes can have?
    def maxHBalancedHeight(n: Int): Int = {
        val (h1, n1) = minHBalancedNodeCounts.dropWhile({ case (h, n1) => n1 < n }).head
        if (n1 == n) h1 else (h1 - 1)
    }

    // what is the minimum height a balanced tree with n nodes must have
    def minHBalancedHeight(n: Int): Int = {
        if (n <= 0) 0
        else math.floor(math.log(n + 1)/math.log(2.0)).toInt
    }

    // p60 - all height balanced trees with a given number of nodes
    // I'm just gonna generate and filter by node count - I'm sure there's some cleverer way of doing this
    def hBalancedTreesWithNodes[T](n: Int, v: T): List[Tree[T]] = 
        if (n <= 0) List()
        else {
            import scala.collection.mutable
            val buf = new mutable.ListBuffer[Tree[T]]
            val minHeight = minHBalancedHeight(n)
            val maxHeight = maxHBalancedHeight(n)
            for{h <- (minHeight to maxHeight)
                t <- hBalancedTrees(h, v).filter(_.nodeCount == n) } { buf += t }

            buf.toList
        }

    def empty[T]:Tree[T] = End

    // problem 63
    // construct a complete binary tree with n nodes
    def completeBinaryTree[T](n: Int, v: T): Tree[T] = {
        def getNode(index: Int): Tree[T] = 
            if (index > n) empty[T]
            else Node(v, getNode(2*index), getNode(2*index + 1))

        getNode(1)
    }
}
