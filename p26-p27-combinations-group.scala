/**
 * Returns a list of all combinations of n items from xs
 */
def combinations[T](n: Int, xs: List[T]): List[List[T]] = {
    val origLen = xs.length
    if (n == 0 || n > origLen) return List(Nil)

    import scala.collection.mutable;
    var combs = new mutable.ListBuffer[List[T]]

    var subList = xs
    var remLen = origLen
    while(remLen >= n){
        val x = subList.head
        subList = subList.drop(1)
        remLen = remLen - 1
        for(subComb <- combinations(n - 1, subList)){
            combs += x :: subComb
        }
    }

    combs.toList
}

/**
 * ns = {n1, n2,...nm} is a list of item counts and xs is a list of items. groupN returns the list of all partitions
 * of xs with each set in the partition containing n1, n2..nm items respectively.
 */
def groupN(ns: List[Int], xs: List[Any]): List[List[List[Any]]] = ns match {
    case Nil => List(Nil) 
    case (n::ns1) => {
        import scala.collection.mutable
        val res = mutable.ListBuffer[List[List[Any]]]()
        for(cs <- combinations(n, xs)){
            val xs1 = xs.diff(cs)
            for(subRes <- groupN(ns1, xs1)) res += (cs :: subRes)
        }

        res.toList
    }
}
