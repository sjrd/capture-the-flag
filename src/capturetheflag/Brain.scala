package capturetheflag

case class Env(
  pos: Pos,
  flag: Option[Team],
  north: SquareInfo,
  east: SquareInfo,
  south: SquareInfo,
  west: SquareInfo,
  northEast: SquareInfo,
  southEast: SquareInfo,
  southWest: SquareInfo,
  northWest: SquareInfo
)

sealed abstract class Action(incX: Int, incY: Int) {
  val inc = (incX, incY)
}

object Action {
  object North extends Action(0, -1)
  object East extends Action(1, 0)
  object South extends Action(0, 1)
  object West extends Action(-1, 0)
  object NorthEast extends Action(1, -1)
  object SouthEast extends Action(1, 1)
  object SouthWest extends Action(-1, 1)
  object NorthWest extends Action(-1, -1)
}

abstract class Brain(val team: Team) {
  type State >: Null <: AnyRef

  def execute(state: State, env: Env): (Option[Action], State)
}

object Brain {
  type NewBrainFun = (Team, Pos, Size) => Brain
}
