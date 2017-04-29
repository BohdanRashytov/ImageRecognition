package ir6_8

object Common {

  sealed trait Diagnosis

  case object Cancer extends Diagnosis

  case object Fibroadenomatosis extends Diagnosis

  case object Indeterminately extends Diagnosis

  case class Cell(diagnosis: Diagnosis = Fibroadenomatosis, observations: List[Double] = List())

  case class Experiment(cell: Cell, samples: List[Cell], diagnosis: Option[Diagnosis])

  val experiments = Data.cells.map(cell => Experiment(cell, Data.cells.filterNot(_.eq(cell)), None))

  var count = 0

  def accuracyCalculate(experiments: List[Experiment], method: Experiment => Experiment): (Double, Double, Double) = {
    val completedExperiment = experiments.map(method)

    val N = completedExperiment.count(_.cell.diagnosis == Fibroadenomatosis)
    val P = completedExperiment.count(_.cell.diagnosis == Cancer)

    val TN = completedExperiment.count(e => e.cell.diagnosis == Fibroadenomatosis && e.diagnosis == Some(Fibroadenomatosis))
    val TP = completedExperiment.count(e => e.cell.diagnosis == Cancer && e.diagnosis == Some(Cancer))

    val TPR = 1.0 * TP / P
    val TNR = 1.0 * TN / N
    val PR = 1.0 * (TP + TN) / (P + N)

    (TPR, TNR, PR)
  }

  import java.lang.Math._

  case class Point(x: Double, y: Double)

  case class Line(ax: Double, by: Double, c: Double)

  case class Circle(center: Point, R: Double)

  case class Ellipse(center: Point, R: Double, coef: Double)

  def searchL(p1: Point, p2: Point): Line =
    Line(ax = 1.0 / (p2.x - p1.x), by = 1.0 / (p1.y - p2.y), c = p1.y / (p2.y - p1.y) - p1.x / (p2.x - p1.x))

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
    points.map(p => Point(x = p.x * cos(angle) - p.y * sin(angle), y = p.x * sin(angle) + p.y * cos(angle)))

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
