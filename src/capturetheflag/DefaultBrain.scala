package capturetheflag

import ozma._

class DefaultBrain(team: Team) extends Brain(team) {
  import Action._

  class State

  val allActions = List(North, East, South, West,
      NorthEast, SouthEast, SouthWest, NorthWest)

  def execute(state: State, env: Env): (Option[Action], State) = {
    val action = Random.rand(allActions)
    (Some(action), state)
  }
}

object DefaultBrain {
  val make: Brain.NewBrainFun = (team, pos, size) => new DefaultBrain(team)
}
