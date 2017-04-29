package ir6_8

object IR6_8 {
  def main(args: Array[String]): Unit = {
    println("---------------------------------------------------------------------------------------------------------")
    println("          TPR     TNR       PR")

    val PetuninConductExperiment = Common.accuracyCalculate(Common.experiments, Petunin.conductExperiment)
    val Petunin_TPR = Math.round(PetuninConductExperiment._1 * 100) / 100.0
    val Petunin_TNR = Math.round(PetuninConductExperiment._2 * 100) / 100.0
    val Petunin_PR = Math.round(PetuninConductExperiment._3 * 100) / 100.0
    println(s"Petunin:  $Petunin_TPR     $Petunin_TNR     $Petunin_PR")

    val PetuninFirstRankConductExperiment = Common.accuracyCalculate(Common.experiments, PetuninFirstRank.conductExperiment)
    val PetuninFirstRank_TPR = PetuninFirstRankConductExperiment._1
    val PetuninFirstRank_TNR = PetuninFirstRankConductExperiment._2
    val PetuninFirstRank_PR = PetuninFirstRankConductExperiment._3
    println(s"PetuninFirstRank:  $PetuninFirstRank_TPR     $PetuninFirstRank_TNR     $PetuninFirstRank_PR")

    val PetuninSecondRankConductExperiment = Common.accuracyCalculate(Common.experiments, PetuninSecondRank.conductExperiment)
    val PetuninSecondRank_TPR = PetuninSecondRankConductExperiment._1
    val PetuninSecondRank_TNR = PetuninSecondRankConductExperiment._2
    val PetuninSecondRank_PR = PetuninSecondRankConductExperiment._3
    println(s"PetuninSecondRank:  $PetuninSecondRank_TPR     $PetuninSecondRank_TNR     $PetuninSecondRank_PR")

    println("---------------------------------------------------------------------------------------------------------")
  }
}
