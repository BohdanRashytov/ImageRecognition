package ir3_8

import Jama.Matrix
import ir3_8.Common.{Cancer, Experiment, Fibroadenomatosis}

object Fisher {

  def conductExperiment(experiment: Experiment): Experiment = {
    val X1 = experiment.samples.filter(_.diagnosis == Cancer).map(_.observations.toArray).toArray
    val X2 = experiment.samples.filter(_.diagnosis == Fibroadenomatosis).map(_.observations.toArray).toArray

    val X_1 = mult(X1.foldLeft(new Array[Double](X1(0).length))((array, el) => plus(array, el)), 1.0 / X1.length)
    val X_2 = mult(X2.foldLeft(new Array[Double](X2(0).length))((array, el) => plus(array, el)), 1.0 / X2.length)

    val S1 = X1.foldLeft(new Matrix(X1(0).length, X1(0).length))((matrix, el) => matrix plus (new Matrix(minus(el, X_1)).transpose() times new Matrix(minus(el, X_1)))) times (1.0 / (X1.length - 1))
    val S2 = X1.foldLeft(new Matrix(X2(0).length, X2(0).length))((matrix, el) => matrix plus (new Matrix(minus(el, X_2)).transpose() times new Matrix(minus(el, X_2)))) times (1.0 / (X2.length - 1))

    val Sw = ((S1 times (X1.length - 1)) plus (S2 times (X2.length - 1))) times (X1.length - 1 + X1.length - 1)
    val Sw_1 = Sw.inverse()

    val w = new Matrix(minus(X_1, X_2)) times Sw_1
    val wT = w.transpose()

    val Y1 = X1.map(el => (new Matrix(el) times wT).get(0, 0))
    val Y2 = X2.map(el => (new Matrix(el) times wT).get(0, 0))

    val Y_1 = Y1.sum / Y1.length
    val Y_2 = Y2.sum / Y2.length
    val Y_1_2 = (Y_1 + Y_2) / 2

    val X0 = experiment.cell.observations.toArray
    val Y0 = (new Matrix(X0) times wT).get(0, 0)

    experiment.copy(diagnosis = Some(if (Y_1_2 > Y0) Fibroadenomatosis else Cancer))
  }

  def minus(arrayOne: Array[Double], arrayTwo: Array[Double]): Array[Double] = {
    val result = new Array[Double](arrayOne.length)
    (0 to result.length - 1).foreach(i => result.update(i, arrayOne(i) - arrayTwo(i)))
    result
  }

  def plus(arrayOne: Array[Double], arrayTwo: Array[Double]): Array[Double] = {
    val result = new Array[Double](arrayOne.length)
    (0 to result.length - 1).foreach(i => result.update(i, arrayOne(i) + arrayTwo(i)))
    result
  }

  def mult(arrayOne: Array[Double], c: Double): Array[Double] = {
    val result = new Array[Double](arrayOne.length)
    (0 to result.length - 1).foreach(i => result.update(i, arrayOne(i) * c))
    result
  }

  implicit def transform(array: Array[Double]): Array[Array[Double]] = {
    val res = new Array[Array[Double]](1)
    res.update(0, array)
    res
  }
}
