package capturetheflag

case class Pos(x: Int, y: Int) {
  def + (that: Pos) = Pos(x + that.x, y + that.y)

  def + (that: Size) = Pos(x + that.width, y + that.height)

  def to(that: Pos) =
    for (x1 <- x to that.x toList; y1 <- y to that.y toList)
      yield Pos(x1, y1)

  def until(that: Pos) = {
    for (x1 <- x to (that.x-1) toList; y1 <- y to (that.y-1) toList)
      yield { val p = Pos(x1, y1); println(p); p }
  }

  def around(n: Int) = {
    for (x1 <- -n to n toList; y1 <- -n to n toList;
        if ((x1 != 0) || (y1 != 0)))
      yield this + (x1, y1)
  }

  override def toString() = "("+x+", "+y+")"
}

object Pos {
  def rectangle(x0: Int, y0: Int, x1: Int, y1: Int) =
    Pos(x0, y0) until Pos(x1, y1)

  def rectangle(topLeft: Pos, size: Size): List[Pos] =
    topLeft until (topLeft + size)
}
