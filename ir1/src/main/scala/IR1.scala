package ir1

object IR1 {
  def main(args: Array[String]) = {
    println("---------------------------------------------------------------------------------------------------------")
    println("Laboratory work #1")
    println("1. Calculation Petunin measures implemented;")

    val genN01 = Generate.N(0, 1)
    println("2. Generate one hundred samples of random variables with distribution N(0,1);")

    val genN31 = Generate.N(3, 1)
    println("3. Generate one hundred samples of random variables with distribution N(3,1);")

    val averageMeasureN01andN31 =
      (0 to Generate.countSamples - 1)
        .map(i => Petunin.measure(genN01(i), genN31(i)))
        .fold(0.0)((a, b) => a + b) / Generate.countSamples

    val averageMeasureN01andN01 =
      (0 to Generate.countSamples - 1)
        .map(i => Petunin.measure(genN01(i),
          genN01(i match {
            case 99 => 0
            case _ => i + 1
          })))
        .fold(0.0)((a, b) => a + b) / Generate.countSamples

    val averageMeasureN31andN31 =
      (0 to Generate.countSamples - 1)
        .map(i => Petunin.measure(genN31(i),
          genN31(i match {
            case 99 => 0
            case _ => i + 1
          })))
        .fold(0.0)((a, b) => a + b) / Generate.countSamples


    println("4. Compactness teza checked, result:")
    println("  Average Petunin measures between ...")
    println("   N(0,1) and N(0,1) equal " + averageMeasureN01andN01)
    println("   N(3,1) and N(3,1) equal " + averageMeasureN31andN31)
    println("   N(0,1) and N(3,1) equal " + averageMeasureN01andN31)
    println(" Tested successfully!!!")
    println("---------------------------------------------------------------------------------------------------------")
  }

}