package ir1

object Petunin {

  val g = 3.0

  def measure(x: List[Double], y: List[Double]): Double = {
    coef(
      N(x.length),
      L(
        Pij(
          HijN(x, y),
          y.length,
          g),
        PijN(x.length),
        x.length)
    )
  }

  def HijN(x: List[Double], y: List[Double]): Map[(Int, Int), Double] = {
    var hijN: Map[(Int, Int), Double] = Map()
    val n = x.size
    for {
      i <- 0 to n - 2
      j <- i + 1 to n - 1
    } yield {
      hijN = hijN.+((i, j) ->
        1.0 * y
          .map(xk => (x(i) < xk & x(j) > xk))
          .map(booleanToInt)
          .fold(0)((a, b) => a + b) / y.size
      )
    }
    hijN
  }

  def Pij(hijN: Map[(Int, Int), Double], m: Double, g: Double): Map[(Int, Int), (Double, Double)] = {
    var pij: Map[(Int, Int), (Double, Double)] = Map()
    hijN.foreach(ijh =>
      pij = pij.+(
        (ijh._1._1, ijh._1._2) ->
          ((ijh._2 * m + g * g / 2 - g * Math.sqrt(ijh._2 * (1 - ijh._2) * m + g * g / 4)) / (m + g * g),
            (ijh._2 * m + g * g / 2 + g * Math.sqrt(ijh._2 * (1 - ijh._2) * m + g * g / 4)) / (m + g * g))
      )
    )
    pij
  }

  def PijN(n: Int): Map[(Int, Int), Double] = {
    var pijN: Map[(Int, Int), Double] = Map()
    for {
      i <- 0 to n - 2
      j <- i + 1 to n - 1
    } yield {
      pijN = pijN.+((i, j) ->
        1.0 * (j - i) / (n + 1)
      )
    }
    pijN
  }

  def N(n: Double): Double = (n * (n - 1) / 2)

  def L(pij: Map[(Int, Int), (Double, Double)],
        pijN: Map[(Int, Int), Double],
        n: Int): Double = {
    var L = 0.0
    for {
      i <- 0 to n - 2
      j <- i + 1 to n - 1
    } yield {
      if (pij(i, j)._1 < pijN(i, j) &
        pij(i, j)._2 > pijN(i, j)) L = L + 1.0
    }
    L
  }

  def coef(N: Double, L: Double) = (L / N)

  def booleanToInt(b: Boolean) = b match {
    case true => 1
    case false => 0
  }
}
