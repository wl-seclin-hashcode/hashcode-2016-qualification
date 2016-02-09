package hashcode

import java.io.File
import java.util.Scanner

object Parser {
  def read(f: String): Problem = {
    val scan = new Scanner(new File(f))
    import scan._
    val nrow = nextInt()
    val ncol = nextInt()
    nextLine()
    val picture = Vector.fill(nrow) { nextLine() }

    Problem(picture, nrow, ncol)
  }
}