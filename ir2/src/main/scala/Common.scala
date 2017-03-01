package ir2

import java.lang.Math._

case class Point(x: Double, y: Double)

case class Line(ax: Double, by: Double, c: Double)

object Common {
  val n = 30
  val m = 100


  def generate(n: Int): List[Point] = {
    (1 to n).map(_ => Point(random(), random())).toList
  }

  def searchL(p1: Point, p2: Point): Line =
    Line(ax = 1 / (p2.x - p1.x), by = 1 / (p1.y - p2.y), c = p1.y / (p2.y - p1.y) - p1.x / (p2.x - p1.x))

  def distance(line: Line, point: Point): Double =
    abs(line.ax * point.x + line.by * point.y + line.c) / sqrt(line.ax * line.ax + line.by * line.by)

  def distance(point1: Point, point2: Point): Double =
    sqrt((point1.x - point2.x) * (point1.x - point2.x) + (point1.y - point2.y) * (point1.y - point2.y))

  def crossingLines(line1: Line, line2: Line): Point =
    Point(x = -(line1.c * line2.by - line2.c * line1.by) / (line1.ax * line2.by - line2.ax * line1.by),
      y = -(line1.ax * line2.c - line2.ax * line1.c) / (line1.ax * line2.by - line2.ax * line1.by))

  def transferPoints(points: List[Point], point: Point): List[Point] =
    points.map(p => Point(p.x - point.x, p.y - point.y))

  def turnPoints(points: List[Point], angle: Double): List[Point] =
    points.map(p => Point(x = p.x*cos(angle) - p.y*sin(angle), y = p.x*sin(angle) + p.y*cos(angle)))

  def searchPkPl(points: List[Point]): (Point, Point) = {
    var max: Double = 0.0
    var findPoints: (Point, Point) = (points(0), points(1))
    points.foreach(p1 => {
      points.foreach(p2 => {
        if ((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y) > max) {
          max = (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y)
          findPoints = (p1, p2)
        }
      })
    })
    findPoints
  }

  def searchPr(points: List[Point], line: Line): Point = {
    var max: Double = distance(line, points(0))
    var findPoint: Point = points(0)
    points.foreach(p => {
      if (distance(line, p) > max && (line.by * p.y > -line.ax * p.x - line.c)) {
        max = distance(line, p)
        findPoint = p
      }
    })
    findPoint
  }

  def searchPq(points: List[Point], line: Line): Point = {
    var max: Double = distance(line, points(0))
    var findPoint: Point = points(0)
    points.foreach(p => {
      if (distance(line, p) > max && (line.by * p.y < -line.ax * p.x - line.c)) {
        max = distance(line, p)
        findPoint = p
      }
    })
    findPoint
  }

  def searchL1orL2(line: Line, point: Point): Line =
    Line(ax = line.ax, by = line.by, c = -line.ax * point.x - line.by * point.y)

  def searchL3orL4(line: Line, point: Point): Line =
    Line(ax = line.by,
      by = -line.ax,
      c = -point.x * line.by + point.y * line.ax)
}
