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
    val g = (-100 to 100).map(i => (1.0 * i / 100, -(line.c + line.ax * 1.0 * i / 100) / line.by)).toList
    plot.addLinePlot("", g.map(q => q._1).toArray, g.map(q => q._2).toArray)
  }

  def paint(points: List[Point]) = {
    plot.addLinePlot("", points.map(q => q.x).toArray, points.map(q => q.y).toArray)
  }

  def paint(circle: Circle) = {
    val g1 = (0 to 100).map(i => (circle.center.x - circle.R + 2.0 * circle.R * i / 100,
      circle.center.y + Math.sqrt(circle.R * circle.R - (circle.center.x - circle.center.x - circle.R + 2.0 * circle.R * i / 100) * (circle.center.x - circle.center.x - circle.R + 2.0 * circle.R * i / 100)))).toList
    val g2 = (0 to 100).map(i => (circle.center.x - circle.R + 2.0 * circle.R * i / 100,
      circle.center.y - Math.sqrt(circle.R * circle.R - (circle.center.x - circle.center.x - circle.R + 2.0 * circle.R * i / 100) * (circle.center.x - circle.center.x - circle.R + 2.0 * circle.R * i / 100)))).toList
    val g = g1.:::(g2.reverse)
    plot.addLinePlot("", g.map(q => q._1).toArray, g.map(q => q._2).toArray)
  }

  def paint(ellipse: Ellipse) = {
    val g1 = (0 to 100).map(i => (ellipse.center.x - ellipse.R + 2.0 * ellipse.R * i / 100,
      ellipse.center.y + Math.sqrt(ellipse.R * ellipse.R - (ellipse.center.x - ellipse.center.x - ellipse.R + 2.0 * ellipse.R * i / 100) * (ellipse.center.x - ellipse.center.x - ellipse.R + 2.0 * ellipse.R * i / 100)))).toList
    val g2 = (0 to 100).map(i => (ellipse.center.x - ellipse.R + 2.0 * ellipse.R * i / 100,
      ellipse.center.y - Math.sqrt(ellipse.R * ellipse.R - (ellipse.center.x - ellipse.center.x - ellipse.R + 2.0 * ellipse.R * i / 100) * (ellipse.center.x - ellipse.center.x - ellipse.R + 2.0 * ellipse.R * i / 100)))).toList
    val g = g1.:::(g2.reverse)
    plot.addLinePlot("", g.map(q => q._1 * ellipse.coef).toArray, g.map(q => q._2).toArray)
  }
}