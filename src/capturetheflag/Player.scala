package capturetheflag

import scala.ozma._

case class Player(val team: Team, val brain: Brain,
    val flag: Option[Team] = None, s: Brain#State = null) {

  private val gamePlay = team.master.gamePlay

  val state: brain.type#State = s.asInstanceOf[brain.type#State]

  def hasFlag = !flag.isEmpty

  def withFlag(flagTeam: Team) = copy(flag = Some(flagTeam))

  def withoutFlag() = copy(flag = None)

  def move(pos: Pos) {
    thread {
      val env = buildEnvironment(pos)
      val (action, newState) = brain.execute(state, env)

      if (action.isEmpty) {
        sleep(10)
        move(pos)
      } else {
        val newPos = pos + action.get.inc
        gamePlay.movePlayer(this, pos, newPos)
      }
    }
  }

  def buildEnvironment(pos: Pos) = {
    Env(pos, flag, null, null, null, null, null, null, null, null)
  }
}
