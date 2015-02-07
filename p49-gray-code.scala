object GrayCodes {
    import scala.collection.mutable
    val codes = new mutable.HashMap[Int, List[String]]

    def gray(n: Int): List[String] = {
       if (codes.contains(n)) return codes(n)
       if (n == 0) return List("")

       val prevCodes = gray(n - 1)
       val buf = new mutable.ListBuffer[String]
       for(str <- prevCodes)
        buf += "0" + str

       for(str <- prevCodes.reverse)
        buf += "1" + str

       val newCodes = buf.toList
       codes(n) = newCodes
       newCodes
    }
}
