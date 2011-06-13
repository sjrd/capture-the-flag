package capturetheflag

import scala.ozma._
import ozma._

import util._

object SquareKind extends Enumeration {
  val Normal = Value("Normal")
  val Bomb = Value("Bomb")
  val Food = Value("Food")
}

class SquareInfo(val home: Option[Team], val kind: SquareKind,
    val player: Option[Team], val flag: Option[Team]) {
  def isHome = !home.isEmpty
  def hasPlayer = !player.isEmpty
  def hasFlag = !flag.isEmpty
}

class Square(val master: Master, val pos: Pos,
    val kind: SquareKind = SquareKind.Normal,
    _home: Option[Team] = None) extends AdvancedPortObject {

  val isHome = !_home.isEmpty
  def home = _home.get

  println(pos + " - " + kind + " - " + isHome)

  def isHomeOf(team: Team) = isHome && (home == team)
  def isHomeOf(player: Player) = isHome && (home == player.team)

  lazy val neighborPos = pos.around(1) filter (master.mapSize.contains)
  lazy val neighbors = neighborPos map (master.squares(_))

  final case class State(
    player: Option[Player],
    flag: Option[Team],
    enabled: Boolean
  ) {
    def hasPlayer = !player.isEmpty

    def hasFlag = !flag.isEmpty

    def isTeamMateOf(team: Team) = hasPlayer && player.get.team == team
    def isTeamMateOf(p: Player): Boolean = isTeamMateOf(p.team)
  }

  protected val initialState = State(None, None, true)

  def quit(): Ack = serialized { state =>
    (Ack, state.copy(player = None))
  }

  def enter(player: Player) = serialized { state =>
    val state1 = if (!state.hasPlayer) state else {
      assert(player.team != state.player.get.team)
      killPlayer(state)
    }

    val state2 = state1.copy(player = Some(player))
    val result = execute(state2)

    thread {
      val (ack, newState) = result
      if (newState.hasPlayer) {
        ack.waitFor()
        newState.player.get.move(pos)
      }
    }

    result
  }

  def killPlayer(state: State) = {
    val droppedFlag = state.player.get.flag
    val state1 = state.copy(player = None)

    if (droppedFlag.isEmpty)
      state1
    else if (isHome) {
      master.flagDropped(droppedFlag.get, home)
      state1.copy(flag = None)
    } else if (state.hasFlag) {
      popFlag(droppedFlag.get)
      state1
    } else {
      state1.copy(flag = droppedFlag)
    }
  }

  def disable(state: State) = {
    val time = kind match {
      case SquareKind.Bomb => master.bombRepopTime
      case SquareKind.Food => master.foodRepopTime
    }

    delayedSerialized(time)(enable)

    state.copy(enabled = false)
  }

  private def enable(state: State) = {
    val state1 = state.copy(enabled = true)

    if (master.isTerminated)
      state
    else if (state.hasPlayer)
      execute(state1)._2
    else
      state1
  }

  private def execute(state: State): (Ack, State) = {
    assert(state.hasPlayer)

    val player = state.player.get

    if ((kind == SquareKind.Food) && (state.enabled)) {
      master.createPlayer(player.team)
      (Ack, disable(state))
    } else if ((kind == SquareKind.Bomb) && state.enabled) {
      val state1 = killPlayer(state)
      (Ack, disable(state1))
    } else if (isHomeOf(player)) {
      val droppedFlag = player.flag
      val newPlayer = player.withoutFlag()
      val newState = state.copy(player = Some(newPlayer))
      val ack = if (droppedFlag.isEmpty) Ack else
        master.flagDropped(droppedFlag.get, home)
      (ack, newState)
    } else if (state.hasFlag && !player.hasFlag) {
      val newPlayer = player.withFlag(state.flag.get)
      (Ack, state.copy(player = Some(newPlayer), flag = None))
    } else {
      (Ack, state)
    }
  }

  def popFlag(flagTeam: Team): Unit = serialized { state =>
    if (!state.hasFlag)
      ((), state.copy(flag = Some(flagTeam)))
    else {
      /* We need a thread here because of potential deadlock between two
       * adjacent squares that process a popFlag at the same time. */
      thread {
        val freeSquares = neighbors filter { neighbor =>
          val info = neighbor.getInfo
          (info.kind == SquareKind.Normal) && (!info.hasPlayer)
        }

        if (freeSquares.isEmpty) {
          sleep(master.timeUnit)
          popFlag(flagTeam)
        } else {
          Random.rand(freeSquares).popFlag(flagTeam)
        }
      }

      ((), state)
    }
  }

  def getInfo = serialized { state =>
    val flag = if (state.hasPlayer) state.player.get.flag else state.flag
    val playerTeam = if (state.hasPlayer) Some(state.player.get.team) else None
    val actualKind = if (state.enabled) kind else SquareKind.Normal

    (new SquareInfo(_home, actualKind, playerTeam, flag), state)
  }

  def getPlayer = serialized { state =>
    (state.player, state)
  }

  def isTeamMateOf(team: Team) = serialized { state =>
    (state.isTeamMateOf(team), state)
  }

  def isTeamMateOf(player: Player) = serialized { state =>
    (state.isTeamMateOf(player), state)
  }
}
