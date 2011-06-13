package object capturetheflag {
  type SquareKind = SquareKind.Value

  implicit def pair2pos(pair: (Int, Int)) = Pos(pair._1, pair._2)
}
