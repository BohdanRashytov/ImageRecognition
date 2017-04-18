package ir3_8

import java.io.{BufferedReader, FileReader}
import java.lang.Math._
import ir3_8.Common._

object Data {

  private case class CellInfo(number: Int = 0, observations: List[Double] = List())

  private case class PatientInfo(count: Int = 0, cells: List[CellInfo] = List())

  private val folderALLD2 = "DIAG/ALLD2/"
  private val folderALLD3 = "DIAG/ALLD3/"

  private val filesALLD2: List[String] = (1 to 9).map(i => s"D20$i.POK").toList ::: (10 to 17).map(i => s"D2$i.POK").toList ::: (19 to 61).map(i => s"D2$i.POK").toList
  private val filesALLD3: List[String] = (1 to 9).map(i => s"D30$i.POK").toList ::: (10 to 42).map(i => s"D3$i.POK").toList

  private val concatALLD2: Map[String, PatientInfo] = filesALLD2.map(name => name -> readFile(folderALLD2 + name)).toMap
  private val concatALLD3: Map[String, PatientInfo] = filesALLD3.map(name => name -> readFile(folderALLD3 + name)).toMap

  private def readNumber(number: String): Double = {
    val n = if (number.startsWith(".")) s"0$number" else number
    if (n.contains("E")) {
      val mainPart = n.split("E")(0).toDouble
      val powPart = n.split("E")(1).toInt
      mainPart * pow(10, powPart)
    } else n.toDouble
  }

  private def readLine(line: String): CellInfo = {
    val elements = line.replace("\t", " ").split(" ").toList.filterNot(_.isEmpty)
    CellInfo(elements.head.toInt, elements.tail.map(readNumber(_)))
  }

  private def readFile(file: String): PatientInfo = {
    val reader = new FileReader(file)
    val bufferReader = new BufferedReader(reader)
    var read = bufferReader.readLine
    val count = read.replace("\t", " ").split(" ").toList.distinct.filterNot(_.isEmpty).head.toInt
    var lines = List[CellInfo]()
    (1 to count).foreach(_ => {
      read = bufferReader.readLine
      lines = lines ::: readLine(read) :: List[CellInfo]()
    })
    PatientInfo(count, lines)
  }

  val cells: List[Cell] = {
    concatALLD2.values.flatMap(pi => pi.cells.map(ci => ci.observations)).map(list => Cell(Cancer, list)).toList :::
      concatALLD3.values.flatMap(pi => pi.cells.map(ci => ci.observations)).map(list => Cell(Fibroadenomatosis, list)).toList
  }
}
