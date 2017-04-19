package ir3_8

object IR3_8 {

  def main(args: Array[String]): Unit = {
    println("---------------------------------------------------------------------------------------------------------")
    println("          TPR     TNR       PR")

    val Fisher_TPR = Math.round(Common.accuracyCalculate(Common.experiments, Fisher.conductExperiment)._1 * 100) / 100.0
    val Fisher_TNR = Math.round(Common.accuracyCalculate(Common.experiments, Fisher.conductExperiment)._2 * 100) / 100.0
    val Fisher_PR = Math.round(Common.accuracyCalculate(Common.experiments, Fisher.conductExperiment)._3 * 100) / 100.0
    println(s"Fisher:  $Fisher_TPR     $Fisher_TNR     $Fisher_PR")

    val HardSVM_TPR = Math.round(Common.accuracyCalculate(Common.experiments, HardSVM.conductExperiment)._1 * 100) / 100.0
    val HardSVM_TNR = Math.round(Common.accuracyCalculate(Common.experiments, HardSVM.conductExperiment)._2 * 100) / 100.0
    val HardSVM_PR = Math.round(Common.accuracyCalculate(Common.experiments, HardSVM.conductExperiment)._3 * 100) / 100.0
    println(s"HardSVM:  $HardSVM_TPR     $HardSVM_TNR     $HardSVM_PR")

    val SoftSVM_TPR = Math.round(Common.accuracyCalculate(Common.experiments, SoftSVM.conductExperiment)._1 * 100) / 100.0
    val SoftSVM_TNR = Math.round(Common.accuracyCalculate(Common.experiments, SoftSVM.conductExperiment)._2 * 100) / 100.0
    val SoftSVM_PR = Math.round(Common.accuracyCalculate(Common.experiments, SoftSVM.conductExperiment)._3 * 100) / 100.0
    println(s"SoftSVM:  $SoftSVM_TPR     $SoftSVM_TNR     $SoftSVM_PR")

    println("---------------------------------------------------------------------------------------------------------")
  }
}
