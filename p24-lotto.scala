def lotto(n: Int, M: Int): List[Int] = {
    import scala.collection.mutable
    val numbers = new mutable.ArrayBuffer[Int]
    numbers ++= (1 to M).toArray

    import scala.util.Random
    val rand = new Random

    val res = new mutable.ListBuffer[Int]
    for(i <- 1 to n){
        val indx = rand.nextInt.abs % numbers.size
        val num = numbers(indx)
        numbers -= num
        res += num
    }

    res.toList
}
