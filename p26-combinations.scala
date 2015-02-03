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
