package hashcode

case class Problem(picture: Vector[String], nrow: Int, ncol: Int) {
  def update(row: Int, col: Int, c: Char) =
    copy(picture = picture.updated(row, picture(row).updated(col, c)))

  def cellsCount = (for {
    row <- picture
    c <- row
    if c == '#'
  } yield 1).sum
}

