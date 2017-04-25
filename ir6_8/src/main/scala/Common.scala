package ir6_8

object Common {

  sealed trait Diagnosis

  case object Cancer extends Diagnosis

  case object Fibroadenomatosis extends Diagnosis

  case object Indeterminately extends Diagnosis

  case class Cell(diagnosis: Diagnosis = Fibroadenomatosis, observations: List[Double] = List())

  case class Experiment(cell: Cell, samples: List[Cell], diagnosis: Option[Diagnosis])

  val experiments = Data.cells.map(cell => Experiment(cell, Data.cells.filterNot(_.eq(cell)), None))

  def accuracyCalculate(experiments: List[Experiment], method: Experiment => Experiment): (Double, Double, Double) = {
    val completedExperiment = experiments.map(method)

    val N = completedExperiment.count(_.cell.diagnosis == Fibroadenomatosis)
    val P = completedExperiment.count(_.cell.diagnosis == Cancer)

    val TN = completedExperiment.count(e => (e.cell.diagnosis == Fibroadenomatosis && e.diagnosis == Some(Fibroadenomatosis)))
    val TP = completedExperiment.count(e => (e.cell.diagnosis == Cancer && e.diagnosis == Some(Cancer)))

    val TPR = 1.0 * TP / P
    val TNR = 1.0 * TN / N
    val PR = 1.0 * (TP + TN) / (P + N)

    (TPR, TNR, PR)
  }
}
