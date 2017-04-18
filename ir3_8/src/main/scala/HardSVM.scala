package ir3_8

import Jama.Matrix
import ir3_8.Common.{Cancer, Experiment, Fibroadenomatosis}

object HardSVM {

  def conductExperiment(experiment: Experiment): Experiment = {
    val X = experiment.samples.map(_.observations.toArray).toArray
    val W = experiment.samples.map(_.diagnosis match {
      case Cancer => 1
      case Fibroadenomatosis => -1
    }).toArray
    val N = X.length

    val J = new Matrix(N - 1, N - 1)

    (0 to N - 2).foreach(i => {
      (0 to N - 2).foreach(j => {
        if (i == j) {
          J.set(i, j, mult(X(i), X(j)))
        } else {
          J.set(i, j, W(i) * W(j) * mult(X(i), X(j)) - W(i) * W(j) * mult(X(i), X(N - 1)))
        }
      })
    })

    val B = new Matrix((0 to N - 2).map(i => 1.0 + W(i) * W(N - 1) * mult(X(i), X(N - 1))).toArray)

    val extraL = (J.inverse() times B.transpose()).transpose().getArray()(0).map(l => if (l < 0) 0.0 else l)
    val L = new Array[Double](extraL.length + 1)
    (0 to L.length - 1).foreach(i => {
      if (i == L.length - 1) L.update(i, -(0 to extraL.length - 1).map(k => extraL(k) * W(k)).sum * W(i))
      else L.update(i, extraL(i))
    })

    val w = (0 to X.length - 1).map(i => mult(X(i), L(i) * W(i))).foldLeft(new Array[Double](X(0).length))((array, el) => plus(array, el))
    val k = (0 to X.length - 1).filterNot(_ == 0.0).head
    val b = W(k) - mult(w, X(k))

    val x = experiment.cell.observations.toArray
    val g: Double = (0 to X.length - 1).map(i => L(i) * W(i) * mult(X(i), x)).sum + b

    experiment.copy(diagnosis = Some(if (g > 0) Cancer else Fibroadenomatosis))
  }

  def mult(arrayOne: Array[Double], arrayTwo: Array[Double]): Double =
    (0 to arrayOne.length - 1).foldLeft(0.0)((s, i) => s + arrayOne(i) * arrayTwo(i))

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
