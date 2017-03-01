package ir2

import ir2.Common._
import java.lang.Math._

object IR2 {
  def main(args: Array[String]): Unit = {
    var points: List[Point] = generate(n)

    val Pk: Point = searchPkPl(points)._1
    val Pl: Point = searchPkPl(points)._2

    val L: Line = searchL(Pk, Pl)

    val Pq: Point = searchPq(points, L)
    val Pr: Point = searchPr(points, L)

    val L1: Line = searchL1orL2(L, Pq)
    val L2: Line = searchL1orL2(L, Pr)

    val L3: Line = searchL3orL4(L, Pl)
    val L4: Line = searchL3orL4(L, Pk)

    val P1: Point = crossingLines(L1, L3)
    val P2: Point = crossingLines(L1, L4)
    val P3: Point = crossingLines(L2, L3)
    val P4: Point = crossingLines(L2, L4)

    val a: Double = distance(P1, P2) min distance(P1, P4)
    val b: Double = distance(P1, P2) max distance(P1, P4)

//    points = transferPoints(points, P1)
//    points = turnPoints(points, atan(-L1.ax/L1.by))

    Graph.paint(L1)
    Graph.paint(L2)
    Graph.paint(L3)
    Graph.paint(L4)
    Graph.paint(points)


  }

}
