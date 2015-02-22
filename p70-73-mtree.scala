package mtree

case class MTree[+T](value: T, children: List[MTree[T]]) {
    override def toString = 
        if (!children.isEmpty) 
            "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
        else
            "M(" + value.toString + ")"

    def nodeCount: Int = 1 + children.map(_.nodeCount).sum

    def toNodeString: String = value.toString + children.map(_.toNodeString).mkString("") + "^"

    // Problem 71
    // The sum of the (depth -1)s of all the trees basically
    def internalPathLength: Int = internalPathLengthImpl(0)

    private def internalPathLengthImpl(myLength: Int): Int = 
        myLength + children.map(_.internalPathLengthImpl(myLength + 1)).sum

    // problem 72
    // Construct the post-order traveral of the tree
    def postOrder: List[T] = 
        children.map(_.postOrder).foldRight(List[T]())((v, accum) => v ::: accum) ::: List(value)

    // problem 73, part 1
    // convert this MTree to a lispy string
    def toLispyString: String = 
        if (children.isEmpty) value.toString
        else "(" + value.toString + " " + children.map(_.toLispyString).mkString(" ") + ")"
}

object MTree {
    def apply[T](value: T):MTree[T] = MTree(value, List())

    // this function assumes that the nodeString was made
    // from a tree of type MTree[Char]
    def fromNodeString(nodeStr: String): MTree[Char] = {
        val toks = nodeStr.toList
        val (mt, toksLeft) = takeTreeFromNodeToks(toks)
        if (!toksLeft.isEmpty) throw new Exception("Couldn't consume all the tokens while creating MTree")
        mt
    }

    def takeTreeFromNodeToks(toks0: List[Char]): (MTree[Char], List[Char]) = 
        toks0 match {
            case Nil => throw new Exception("takeTreeFromNodeToks encountered an empty list")
            case ('^'::_) => throw new Exception("Encountered an unexpected '^'")
            case (v::toks1) => {
                var toks = toks1               

                import scala.collection.mutable
                val buf = new mutable.ListBuffer[MTree[Char]]

                while(toks.head != '^'){
                    val (nextTree, nextToks) = takeTreeFromNodeToks(toks)
                    buf += nextTree
                    toks = nextToks
                }

                (MTree(v, buf.toList), toks.tail)
            }
        }

    // problem 73 - part II
    // re-create am MTree from a lispyString representation
    // assume that all values are characters for now
    def fromLispyString(lStr: String): MTree[Char] = {
        val toks = lStr.toList.filter(_ != ' ')
        val (tree, toksLeft) = takeTreeFromLispyToks(toks)
        if (!toksLeft.isEmpty) throw new Exception("fromLispyString couldn't consume all tokens!")
        tree
    }

    def takeTreeFromLispyToks(toks0: List[Char]): (MTree[Char], List[Char]) = 
        toks0 match {
            case '('::toks1 =>  // read till we encounter a closing bracket
                toks1 match {
                    case v::toks2 => {
                        import scala.collection.mutable
                        val buf = new mutable.ListBuffer[MTree[Char]]

                        var toks = toks2
                        while(toks.head != ')'){
                            val (tree, nextToks) = takeTreeFromLispyToks(toks)
                            toks = nextToks
                            buf += tree
                        }

                        (MTree(v, buf.toList), toks.tail)
                    }

                    case _ => throw new Exception("Couldn't parse starting from: " + toks1.toString)
                }

            case Nil | (')'::_) => throw new Exception("Failed to parse starting from: " + toks0.toString)

            case v::rest => (MTree(v, List()), rest)
        }
}
