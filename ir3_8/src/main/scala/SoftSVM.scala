package ir3_8

import Jama.Matrix
import ir3_8.Common.{Cancer, Experiment, Fibroadenomatosis}

object SoftSVM {

  val C = 0.01
  val d = 2.0

  def conductExperiment(experiment: Experiment): Experiment = {
    val X = experiment.samples.map(_.observations.toArray).toArray
    val W = experiment.samples.map(_.diagnosis match {
      case Cancer => 1
      case Fibroadenomatosis => -1
    }).toArray
    val N = X.length

    val J = new Matrix(N - 1, N - 1)

    (0 to N - 2).foreach(i => {
      (0 to N-2).foreach(j => {
        if (i == j) {
          J.set(i, j, K(X(i), X(j)) + Kronecker(i,j) / C)
        } else {
          J.set(i, j, W(i)* W(j)*(K(X(i), X(j)) + Kronecker(i,j) / C) -
            W(i)* W(j)*(K(X(i), X(N-1)) + Kronecker(i,N-1) / C))
        }
      })
    })

    val B = new Matrix((0 to N - 2).map(i => 1.0 + W(i)*W(N-1)*(K(X(i), X(N-1))+ Kronecker(i,N-1) / C)).toArray)

    val extraL = (J.inverse() times B.transpose()).transpose().getArray()(0).map(l => if (l < 0) 0.0 else l)
    val L = new Array[Double](extraL.length + 1)
    (0 to L.length - 1).foreach(i => {
      if (i == L.length - 1) L.update(i, - (0 to extraL.length - 1).map(k => extraL(k)*W(k)).sum * W(i))
      else L.update(i, extraL(i))
    })

    val k = (0 to X.length - 1).filterNot(_==0.0).head
    val b = W(k) - (0 to X.length - 1).map(i => L(i)*W(i)*(K(X(i), X(k)) + Kronecker(i, k)/C)).sum

    val x = experiment.cell.observations.toArray
    val g: Double = (0 to X.length - 1).map(i => L(i)*W(i)*K(X(i), x)).sum + b

    experiment.copy(diagnosis = Some(if (g > 0) Cancer else Fibroadenomatosis))
  }

  def K (arrayOne: Array[Double], arrayTwo: Array[Double]): Double =
    Math.pow((0 to arrayOne.length - 1).foldLeft(0.0)((s, i) => s + arrayOne(i)*arrayTwo(i)) + 1, d)

  def Kronecker(i: Int, j: Int): Double = if (i == j) 1.0 else 0.0

  def mult(arrayOne: Array[Double], c: Double): Array[Double] = {
    val result = new Array[Double](arrayOne.length)
    (0 to result.length - 1).foreach(i => result.update(i, arrayOne(i) * c))
    result
  }

  def plus(arrayOne: Array[Double], arrayTwo: Array[Double]): Array[Double] = {
    val result = new Array[Double](arrayOne.length)
    (0 to result.length - 1).foreach(i => result.update(i, arrayOne(i) + arrayTwo(i)))
    result
  }

  implicit def transform(array: Array[Double]): Array[Array[Double]] = {
    val res = new Array[Array[Double]](1)
    res.update(0, array)
    res
  }
}
