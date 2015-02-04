// returns the GCD of two given numbers through the Euclidean algorithm
def gcd(a: Int, b: Int): Int = if (a % b == 0) b else gcd(b, a % b)
