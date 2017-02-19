package ir1

import java.lang.Math._

object Generate {

  val countRecords = 100
  val countSamples = 100

  def N(a: Double, s: Double): List[List[Double]] = {
    (for (_ <- 0 to countSamples - 1)
      yield (0 to countRecords - 1).map(_ =>
        (a + s * sqrt(-2 * log(random())) * cos(2 * PI * random())))
        .toList.sorted).toList
  }
}
