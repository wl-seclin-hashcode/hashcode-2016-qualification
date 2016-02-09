package hashcode

import java.awt.{ Color, Dimension, Graphics }

import grizzled.slf4j.Logging

object Main extends App with Logging {
  val names = Seq(
    "learn_and_teach.in",
    "logo.in",
    "right_angle.in")
  info(names.map(solveIt).sum + s" total score for $names ")

  def solveIt(n: String) = {
    info(s"starting problem $n")
    val problem = Parser.read(n)
    val solution = Solver.solve(problem)
    showSolution(solution, problem, n)

    Validator.score(solution, problem)
      .recover { case e => error(s"validation error for $n", e); 0 }
      .map { score =>
        info(s"score for $n : $score")
        Formatter.write(solution, score, n)
        score
      }.get
  }

  def showSolution(s: Solution, p: Problem, name: String) {

    def paintSol(g: Graphics, d: Dimension, p: Problem, step: Int): Unit = {
      val cellWidth = (d.getWidth / p.ncol).toInt
      val cellHeight = (d.getHeight / p.nrow).toInt
      def coords(x: Int, y: Int): (Int, Int) =
        ((x * cellWidth).toInt,
          (y * cellHeight).toInt)

      for {
        cmd <- s.commands.take(step)
      } draw(cmd)

      def drawCell(r: Int, c: Int, erase: Boolean = false) = {
        val (x, y) = coords(c, r)
        if (erase) g.setColor(Color.white)
        else g.setColor(Color.BLACK)
        g.drawRect(x, y, cellWidth, cellHeight)
      }

      def draw(cmd: Command) = cmd match {
        case PaintSquare(Point(row, col), size) =>
          for {
            r <- row - size to row + size
            c <- col - size to col + size
          } drawCell(r, c)
        case PaintLine(Point(row, col), Point(row2, col2)) =>
          for {
            r <- row to row2
            c <- col to col2
          } drawCell(r, c)
        case Erase(row, col) =>
          drawCell(row, col, true)
      }

    }

    Visualizer(paintSol, (0 to s.commands.size).toSeq, p, name)

  }

}