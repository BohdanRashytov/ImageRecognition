package ir3_8

object IR3_8 {

  def main(args: Array[String]): Unit = {
    println("---------------------------------------------------------------------------------------------------------")
    println("*****Result of accuracy*****")

    val FisherAccuracy = Math.round(Common.accuracyCalculate(Common.experiments, Fisher.conductExperiment) * 100) / 100.0
    println(s"Fisher: $FisherAccuracy")

    val HardSVMAccuracy = Math.round(Common.accuracyCalculate(Common.experiments, HardSVM.conductExperiment) * 100) / 100.0
    println(s"HardSVMA: $HardSVMAccuracy")

    val SoftSVMAccuracy = Math.round(Common.accuracyCalculate(Common.experiments, SoftSVM.conductExperiment) * 100) / 100.0
    println(s"HardSVMA: $SoftSVMAccuracy")


    println("---------------------------------------------------------------------------------------------------------")
  }
}
