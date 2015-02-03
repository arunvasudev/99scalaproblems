def randomSelect[T](n: Int, xs: List[T]): List[T] = {
    val len = xs.length
    if (n > len) return xs

    val xsArray = xs.toIndexedSeq

    import scala.collection.mutable
    val indices = new mutable.ArrayBuffer[Int]
    indices ++= (0 to (len - 1)).toArray

    import scala.util.Random
    val rand = new Random
    val res = new mutable.ListBuffer[T]
    for(i <- 1 to n){
        val indxIndx = rand.nextInt.abs % indices.size
        val indx = indices(indxIndx)
        indices -= indx
        res += xsArray(indx)
    }

    res.toList
}
