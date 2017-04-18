package ir3_8

object Common {

  sealed trait Diagnosis

  case object Cancer extends Diagnosis

  case object Fibroadenomatosis extends Diagnosis

  case class Cell(diagnosis: Diagnosis = Fibroadenomatosis, observations: List[Double] = List())

  case class Experiment(cell: Cell, samples: List[Cell], diagnosis: Option[Diagnosis])

  val experiments = Data.cells.map(cell => Experiment(cell, Data.cells.filterNot(_.eq(cell)), None))

  def accuracyCalculate(experiments: List[Experiment], method: Experiment => Experiment): Double = {
    val completedExperiment = experiments.map(method)
    val successful = completedExperiment.count(e => e.diagnosis.get == e.cell.diagnosis)
    val all = experiments.size
    val accuracy = 1.0 * successful / all
    accuracy
  }
}
