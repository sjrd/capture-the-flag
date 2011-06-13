package capturetheflag

class DefaultBrain(team: Team) extends Brain(team) {
  class State

  def execute(state: State, env: Env): (Option[Action], State) = {
    (None, state)
  }
}

object DefaultBrain {
  val make: Brain.NewBrainFun = (team, pos, size) => new DefaultBrain(team)
}
