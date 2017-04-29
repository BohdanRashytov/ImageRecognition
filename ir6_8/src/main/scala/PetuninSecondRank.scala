package ir6_8

import java.lang.Math.{PI, atan}

import ir6_8.Common._

object PetuninSecondRank {
  def conductExperiment(experiment: Experiment): Experiment = {
    val cell = experiment.cell
    val samples = experiment.samples

    val extraExperiments: List[Experiment] =
      (for {
        i <- 0 to 13
        j <- i + 1 to 14
      } yield {
        conductExtraExperiment(
          Experiment(cell.copy(observations = cell.observations(i) :: cell.observations(j) :: Nil),
            samples.map(c => c.copy(observations = c.observations(i) :: c.observations(j) :: Nil)),
            None))
      }).toList

    val countCancer = extraExperiments.count(_.diagnosis == Some(Cancer))
    val countFibroadenomatosis = extraExperiments.count(_.diagnosis == Some(Fibroadenomatosis))

    val diagnosis = if (countCancer > countFibroadenomatosis) Cancer
    else if (countCancer == countFibroadenomatosis) Indeterminately
    else Fibroadenomatosis

    experiment.copy(diagnosis = Some(diagnosis))
  }

  private def conductExtraExperiment(experiment: Experiment): Experiment = {
    if (experiment.samples.size < 1250) return experiment.copy(diagnosis = Some(Indeterminately))
    val pointCell = Point(experiment.cell.observations(0), experiment.cell.observations(1))
    val pointsCancer = experiment.samples.filter(_.diagnosis == Cancer).map(c => Point(c.observations(0), c.observations(1)))
    val pointsFibroadenomatosis = experiment.samples.filter(_.diagnosis == Fibroadenomatosis).map(c => Point(c.observations(0), c.observations(1)))

    val getCancerInfo = calculate(pointsCancer)
    val getFibroadenomatosisInfo = calculate(pointsFibroadenomatosis)

    var cellCanser = turnPoints(transferPoints(List(pointCell), getCancerInfo._3), getCancerInfo._2).head
    var cellFibroadenomatosis = turnPoints(transferPoints(List(pointCell), getFibroadenomatosisInfo._3), getFibroadenomatosisInfo._2).head

    cellCanser = cellCanser.copy(x = cellCanser.x / getCancerInfo._1.coef)
    cellFibroadenomatosis = cellFibroadenomatosis.copy(x = cellFibroadenomatosis.x / getFibroadenomatosisInfo._1.coef)

    val cancerIndicator = (cellCanser.x - getCancerInfo._1.center.x) * (cellCanser.x - getCancerInfo._1.center.x) +
      (cellCanser.y - getCancerInfo._1.center.y) * (cellCanser.y - getCancerInfo._1.center.y) <= getCancerInfo._1.R

    val fibroadenomatosisIndicator = (cellCanser.x - getFibroadenomatosisInfo._1.center.x) * (cellCanser.x - getFibroadenomatosisInfo._1.center.x) +
      (cellCanser.y - getFibroadenomatosisInfo._1.center.y) * (cellCanser.y - getFibroadenomatosisInfo._1.center.y) <= getFibroadenomatosisInfo._1.R

    val diagnosis = if (cancerIndicator && !fibroadenomatosisIndicator) Cancer else
    if (fibroadenomatosisIndicator && !cancerIndicator) Fibroadenomatosis
    else Indeterminately

    if (diagnosis == Indeterminately) {
      val listCancer = (0 to getCancerInfo._4.size - 1).map(i =>
        (Math.abs((getCancerInfo._4(i).x - getCancerInfo._1.center.x) * (getCancerInfo._4(i).x - getCancerInfo._1.center.x) +
            (getCancerInfo._4(i).y - getCancerInfo._1.center.y) * (getCancerInfo._4(i).y - getCancerInfo._1.center.y) - getCancerInfo._1.R),
          experiment.samples.filter(_.diagnosis == Cancer)(i))).sortBy(_._1).drop(25).map(_._2).toList

      val listFibroadenomatosis = (0 to getFibroadenomatosisInfo._4.size - 1).map(i =>
        (Math.abs((getFibroadenomatosisInfo._4(i).x - getFibroadenomatosisInfo._1.center.x) * (getFibroadenomatosisInfo._4(i).x - getFibroadenomatosisInfo._1.center.x) +
          (getFibroadenomatosisInfo._4(i).y - getFibroadenomatosisInfo._1.center.y) * (getFibroadenomatosisInfo._4(i).y - getFibroadenomatosisInfo._1.center.y) - getFibroadenomatosisInfo._1.R),
          experiment.samples.filter(_.diagnosis == Fibroadenomatosis)(i))).sortBy(_._1).drop(25).map(_._2).toList

      conductExtraExperiment(experiment.copy(samples = listCancer ::: listFibroadenomatosis))}
    else experiment.copy(diagnosis = Some(diagnosis))
  }

  private def calculate(p: List[Point]) = {
    var points: List[Point] = p

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
        min = distance(P2, Point(0, 0))
        point = P2
      }
      if (min > distance(P3, Point(0, 0))) {
        min = distance(P3, Point(0, 0))
        point = P3
      }
      if (min > distance(P4, Point(0, 0))) {
        min = distance(P4, Point(0, 0))
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

    (Ellipse(center, R, 1 / coef), angle, transferPoint, points)
  }

}
