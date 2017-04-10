package ir3_8

object Common {

  sealed trait Diagnosis
  case object Cancer extends Diagnosis
  case object Fibroadenomatosis extends Diagnosis

  case class Cell (diagnosis: Diagnosis = Fibroadenomatosis, observations: List[Double] = List())

}
