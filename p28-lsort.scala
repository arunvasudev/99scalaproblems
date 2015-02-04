/**
 * Sort a list of lists by the length of its element lists
 */
def lsort(xs: List[List[Any]]): List[List[Any]] = xs.sortBy(_.length)

import scala.collection.mutable
def countFreqs[T](xs: List[T]): mutable.HashMap[T, Int] = {
    val m = mutable.HashMap[T, Int]()
    xs.foreach( x => m(x) = if (!m.contains(x)) 1 else m(x) + 1)
    m
}

/*
 * Sorts a list of lists by the frequency of the lengths of the element lists. The list with the
 * lowest occurring frequency comes first, and so on.
 */
def lsortFreq(xs: List[List[Any]]): List[List[Any]] = {
    val lengths = xs.map(_.length).sorted
    val freqs = countFreqs(lengths)
    xs.sortBy(x => (freqs(x.length), x.length))
}
