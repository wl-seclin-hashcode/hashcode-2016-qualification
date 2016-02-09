package hashcode

import java.io.PrintStream
import grizzled.slf4j.Logging

object Formatter extends Logging {
  def write(solution: Solution, score: Int, name: String): Unit = {
    val file = s"out.$name.$score.txt"
    val f = new PrintStream(file)
    f.println(solution.commands.size)
    f.println(solution.commands.mkString("\n"))
    f.close
    info(s"wrote to $file")
  }
}