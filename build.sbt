import sbt.Keys.libraryDependencies

name := "ImageRecognition"

version := "1.0"

scalaVersion := "2.12.1"

lazy val root = (project in file("."))
  .settings(
    name := "root",
    scalaVersion := "2.12.1",
    libraryDependencies += "com.github.yannrichet" % "JMathPlot" % "1.0.1" from "https://github.com/yannrichet/jmathplot/blob/master/dist/jmathplot.jar"
  )

lazy val ir1 = (project in file ("ir1"))

lazy val ir2 = (project in file ("ir2"))
  .settings(
    libraryDependencies += "com.github.yannrichet" % "JMathPlot" % "1.0.1" from "https://github.com/yannrichet/jmathplot/blob/master/dist/jmathplot.jar"
  )

lazy val ir3_5 = (project in file ("ir3_5"))
  .settings(
    libraryDependencies += "com.github.yannrichet" % "JMathPlot" % "1.0.1" from "https://github.com/yannrichet/jmathplot/blob/master/dist/jmathplot.jar"
  )

lazy val ir6_8 = (project in file ("ir6_8"))
  .settings(
    libraryDependencies += "com.github.yannrichet" % "JMathPlot" % "1.0.1" from "https://github.com/yannrichet/jmathplot/blob/master/dist/jmathplot.jar"
  )




    