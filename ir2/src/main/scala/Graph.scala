package ir2

import javax.swing.JFrame

import org.math.plot.Plot2DPanel

object Graph {
  var plot: Plot2DPanel = new Plot2DPanel()

  val frame: JFrame = new JFrame("Plot panel")
  frame.setSize(600, 600)
  frame.setVisible(true)
  frame.setContentPane(plot)


  def paint(line: Line) = {
    val g = (-100 to 100).map(i => (1.0 * i / 100, -(-line.c + line.ax * 1.0 * i / 100) / line.by)).toList
    plot.addLinePlot("", g.map(q => q._1).toArray, g.map(q => q._2).toArray)
  }

  def paint(points: List[Point]) = {
    plot.addLinePlot("", points.map(q => q.x).toArray, points.map(q => q.y).toArray)
  }
}