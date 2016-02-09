package hashcode

import grizzled.slf4j.Logging

object Solver extends Logging {
  def solve(problem: Problem): Solution = {
    import problem._

    def paintArea(halfLength: Int, stop: Int, full: Boolean, partialSolution: Problem): (IndexedSeq[Command], Problem) = {
      val length = 2 * halfLength + 1
      def shouldPaint(area: Vector[String]) = {
        val toPaint = area.map(_.count('#'.==)).sum
        val toErase = area.map(_.count('#'.!=)).sum
        if (full)
          area.forall(_.forall('#'.==))
        else
          toErase * 10 < toPaint
        //          toPaint - toErase > 4 * halfLength * halfLength
        //          area.map(_.count('#'.==)).sum > halfLength * halfLength * area.map(_.count('#'.!=)).sum + 1
      }

      if (halfLength <= stop) (IndexedSeq.empty[Command], partialSolution)
      else {
        var rest = partialSolution
        val commands = for {
          row ← 0 until nrow
          if row + length <= nrow
          col ← 0 until ncol
          if col + length <= ncol
        } yield {
          val area = rest.picture
            .slice(row, row + length)
            .map(_.slice(col, col + length))

          if (area.nonEmpty && shouldPaint(area) && area.length == area.head.length) {
            val erases = for {
              r1 ← area.indices
              c1 ← area(r1).indices
              if area(r1)(c1) == '.'
            } yield Erase(row + r1, col + c1)

            for {
              r1 ← area.indices
              c1 ← area(r1).indices
              if area(r1)(c1) == '#'
            } rest = rest.update(row + r1, col + c1, '0')

            PaintSquare(Point(row + halfLength, col + halfLength), halfLength) +: erases
          } else IndexedSeq.empty[Command]
        }
        val cmds = commands.flatten
        val (paints, erases) = cmds.partition { case _: PaintSquare => true; case _ => false }
        debug(s"${paints.size} paints and ${erases.size} erases for size $halfLength")
        val (nextCmds, notPainted) =
//          if (full) paintArea(halfLength, stop, !full, rest)
//          else 
            paintArea(halfLength - 1, stop, full, rest)
        (cmds ++ nextCmds, notPainted)
      }
    }

    val (squareCommands, notPainted) = paintArea(8, 3, true, problem)
    val lineCmds = lineCommands(notPainted, squareCommands)
    Solution(lineCmds)
  }

  def lineCommands(partialSolution: Problem, acc: Seq[Command]): Seq[Command] = {
    import partialSolution._
    (for {
      row ← 0 until nrow
      col ← 0 until ncol
      if picture(row)(col) == '#'
    } yield (row, col)).headOption match {
      case Some((row, col)) =>
        val line = picture(row).drop(col).takeWhile(_ != '.')
        val column = picture.map(_(col)).drop(row).takeWhile(_ != '.')
        val endOfLine = if (line.size > column.size)
          Point(row, col + line.size - 1)
        else Point(row + column.size - 1, col)
        val cmd = PaintLine(Point(row, col), endOfLine)
        val rest = (row to endOfLine.row).foldLeft(partialSolution) { (r, row) =>
          (col to endOfLine.col).foldLeft(r) { (r, col) =>
            r.update(row, col, '0')
          }
        }
        lineCommands(rest, acc :+ cmd)
      case None =>
        acc
    }

  }
}