package ir2

import java.io.{BufferedWriter, FileWriter}

import ir2.Common._
import java.lang.Math._

object IR2 {
  var points: List[Point] = Nil
  var ellipse: Ellipse = Ellipse(Point(0, 0), 0, 1)

  val outputPath = "Output.txt"
  val writer = new FileWriter(outputPath)
  val bufferWriter = new BufferedWriter(writer)

  def main(args: Array[String]): Unit = {
    // #paint
    calculate
    Graph.paint(points)
    Graph.paint(ellipse)

    // #result
    bufferWriter.write("N\t\t\tM\t\t\tP\t\t\t\tF\n")
    (30 to 100 by 10).foreach(N => {
      (100 to 1000 by 100).foreach(M => {
        Common.n = N
        Common.m = M
        val newPoint = generate(m)
        calculate
        val count = newPoint.map(p => (p.x / ellipse.coef - ellipse.center.x) * (p.x / ellipse.coef - ellipse.center.x) +
          (p.y - ellipse.center.y) * (p.y - ellipse.center.y) < ellipse.R * ellipse.R).count(b => b)
        val p = ((1.0 * (n - 1) / (n + 1) * 1000).toInt * 1.0) / 1000.0
        val f = ((1.0 * count / m * 1000).toInt * 1.0) / 1000.0
        if (m != 1000) bufferWriter.write(s"$n\t\t\t$m\t\t\t$p\t\t\t$f\n")
        else bufferWriter.write(s"$n\t\t\t$m\t\t$p\t\t\t$f\n")
      })
    })
    bufferWriter.flush()
  }

  def calculate = {
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
    val P3: Point = crossingLines(L2, L4)
    val P4: Point = crossingLines(L2, L3)

    val a: Double = distance(P1, P2) min distance(P1, P4)
    val b: Double = distance(P1, P2) max distance(P1, P4)

    val transferPoint = {
      var min = distance(P1, Point(0, 0))
      var point = P1
      if (min > distance(P2, Point(0, 0))) {
        min = distance(P2, Point(0, 0));
        point = P2
      }
      if (min > distance(P3, Point(0, 0))) {
        min = distance(P3, Point(0, 0));
        point = P3
      }
      if (min > distance(P4, Point(0, 0))) {
        min = distance(P4, Point(0, 0));
        point = P4
      }
      point
    }
    val angle = if (transferPoint.y > 0) PI / 2 - atan(-L1.ax / L1.by max -L3.ax / L3.by)
    else PI * 2 - atan(-L1.ax / L1.by max -L3.ax / L3.by)

    points = transferPoints(points, transferPoint)
    points = turnPoints(points, angle)

    val coef = {
      val minX = points.map(p => p.x).min
      val maxX = points.map(p => p.x).max
      val minY = points.map(p => p.y).min
      val maxY = points.map(p => p.y).max
      if (maxX - minX > maxY - minX) a / b
      else b / a
    }
    points = points.map(p => Point(p.x * coef, p.y))

    val center: Point = if (coef == a / b) Point(a / 2, a / 2) else Point(b / 2, b / 2)
    val R: Double = points.map(p => distance(p, center)).max
    points = points.map(p => Point(p.x / coef, p.y))

    IR2.points = points
    IR2.ellipse = Ellipse(center, R, 1 / coef)
  }

}
