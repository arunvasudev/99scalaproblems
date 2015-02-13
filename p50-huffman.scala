package huffman

// problem 50 - Huffman encode a list of (symbol, frequency) pairs
object Huffman {
    def encode(syms: List[(String, Int)]): List[(String, String)] = {

        class Tree[+A](val freq: Int)
        case class Branch[A](left: Tree[A], right: Tree[A], override val freq: Int) extends Tree[A](freq)
        case class Leaf[A](value: A, override val freq: Int) extends Tree[A](freq)

        class TreeOrder[A] extends Ordering[Tree[A]]{
            override def compare(a1: Tree[A], a2: Tree[A]): Int = a2.freq.compare(a1.freq)
        }

        import scala.collection.mutable
            val pq = new mutable.PriorityQueue[Tree[String]]()(new TreeOrder[String])

        syms.foldLeft(pq){(accum, pair) => pair match { case (value, freq) =>
                accum += Leaf(value, freq)
                accum
            }
        }

        while(pq.size > 1){
            val t1 = pq.dequeue;
            val t2 = pq.dequeue;
            pq += Branch(t1, t2, t1.freq + t2.freq)
        }

        def encodeTree(t: Tree[String]): List[(String, String)] = t match {
            case Leaf(a, _) => List((a, ""))
            case Branch(left, right, _) => {
                import scala.collection.mutable
                val buf = new mutable.ListBuffer[(String, String)]

                for{ (list, prefix) <- List(encodeTree(left), encodeTree(right)).zip(List("0", "1"))
                      (value, code) <- list }
                   { buf += ((value, prefix + code)) }

                buf.toList
            }
        }

        encodeTree(pq.dequeue)
    }
}
