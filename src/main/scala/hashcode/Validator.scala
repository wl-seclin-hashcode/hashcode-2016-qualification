package hashcode

import scala.annotation.tailrec
import scala.util.Try

object Validator {
  def score(solution: Solution, problem: Problem): Try[Int] = Try {
    validate(solution.commands.toList, problem)
    problem.ncol * problem.nrow - solution.commands.size
  }

  def validate(sol: List[Command], problem: Problem) {
    val painted = paintRec(sol, Map.empty.withDefaultValue(false), problem)

    for {
      i <- 0 until problem.nrow
      line = problem.picture(i)
      j <- 0 until problem.ncol
      c = line(j)
      shouldPaint = c == '#'
      isPainted = painted(Point(i, j))
      ok = isPainted == shouldPaint
    } assert(ok, s"($i,$j) should be $shouldPaint but was $isPainted ($c)")
  }

  @tailrec
  def paintRec(
    sol: List[Command],
    picture: Map[Point, Boolean],
    problem: Problem): Map[Point, Boolean] = sol match {
    case Nil             => picture
    case command :: tail => paintRec(tail, command.update(picture, problem), problem)
  }
}