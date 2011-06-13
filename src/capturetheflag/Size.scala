package capturetheflag

case class Size(width: Int, height: Int) {
  def contains(pos: Pos) =
    (pos.x >= 0) && (pos.x < width) && (pos.y >= 0) && (pos.y < height)
}
