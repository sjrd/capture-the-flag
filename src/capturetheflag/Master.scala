package capturetheflag

import ozma._

import util._

class Master(settings: Settings) extends AdvancedPortObject {
  val mapWidth = settings.width
  val mapHeight = settings.height
  val mapSize = Size(mapWidth, mapHeight)

  val baseWidth = mapWidth
  val baseHeight = 2
  val baseSize = Size(baseWidth, baseHeight)

  val homeWidth = 3
  val homeHeight = baseHeight
  val homeSize = Size(homeWidth, homeHeight)

  val centerSize = Size(mapWidth, mapHeight-2*baseHeight)

  val baseAPos = Pos(0, 0)
  val baseBPos = Pos(0, mapHeight - baseHeight)

  val baseAPositions = Pos.rectangle(baseAPos, baseSize)
  val baseBPositions = Pos.rectangle(baseBPos, baseSize)

  val homeAPositions = Pos.rectangle(
      Pos(Random.rand(mapWidth - homeWidth), 0), homeSize)
  val homeBPositions = Pos.rectangle(
      Pos(Random.rand(mapWidth - homeWidth), mapHeight - homeHeight), homeSize)

  val flagsAPositions = baseAPositions filterNot (homeAPositions.contains)
  val flagsBPositions = baseBPositions filterNot (homeBPositions.contains)

  val teamA = new Team(this, "A", DefaultBrain.make)
  val teamB = new Team(this, "B", DefaultBrain.make)

  val timeUnit = settings.timeUnit
  val bombRepopTime = settings.bombTime * timeUnit
  val foodRepopTime = settings.foodTime * timeUnit
  val flagRepopTime = settings.flagTime * timeUnit
  val cleaningTime = settings.cleaningTime * timeUnit

  val flagsPerTeam = settings.flagsPerTeam

  val gamePlay = new GamePlay(this)

  private val _squares = makeSquares()

  def squares(pos: Pos): Square =
    _squares(pos.x * mapHeight + pos.y)

  final case class State(
    capturedFlagsByA: Int,
    capturedFlagsByB: Int,
    terminated: Boolean,
    winner: Option[Team]
  ) {
    def incCapturedFlags(team: Team) = team match {
      case `teamA` => copy(capturedFlagsByA = capturedFlagsByA + 1).checkWinner
      case `teamB` => copy(capturedFlagsByB = capturedFlagsByB + 1).checkWinner
    }

    def checkWinner = {
      if (capturedFlagsByA == settings.flagsPerTeam)
        copy(winner = Some(teamA), terminated = true)
      else if (capturedFlagsByB == settings.flagsPerTeam)
        copy(winner = Some(teamB), terminated = true)
      else
        this
    }
  }

  protected def initialState = State(0, 0, false, None)

  private def makeSquares() = {
    val centerPositions = Pos.rectangle((0, baseHeight), centerSize)
    val shuffled = centerPositions.sort((a, b) => Random.rand(2) == 0)

    val (bombsPositions, shuffled1) = shuffled.splitAt(settings.bombs)
    val foodsPositions = shuffled1.take(settings.foods)

    for (pos <- Pos.rectangle(0, 0, mapWidth, mapHeight)) yield {
      if (homeAPositions contains pos)
        new Square(this, pos, _home = Some(teamA))
      else if (homeBPositions contains pos)
        new Square(this, pos, _home = Some(teamB))
      else if (bombsPositions contains pos)
        new Square(this, pos, kind = SquareKind.Bomb)
      else if (foodsPositions contains pos)
        new Square(this, pos, kind = SquareKind.Food)
      else
        new Square(this, pos)
    }
  }

  private def newPlayer(team: Team, pos: Pos) =
    team.newPlayer(pos)

  def start() {
    for (i <- 1 to settings.flagsPerTeam) {
      popFlag(teamA)
      popFlag(teamB)
    }

    for (i <- 1 to settings.playersPerTeam) {
      addPlayerInternal(teamA)
      addPlayerInternal(teamB)
    }
  }

  def createPlayer(team: Team) {
    gamePlay.synchronize {
      addPlayer(team)
    }
  }

  private def addPlayer(team: Team) {
    addPlayerInternal(team).waitFor()
  }

  private def addPlayerInternal(team: Team) = serialized { state =>
    val pos = findFreeSquare(team match {
      case `teamA` => homeAPositions
      case `teamB` => homeBPositions
    })

    if (pos.isEmpty) {
      // Try again later
      gamePlay.delaySynchronize(timeUnit) {
        addPlayer(team)
      }
    } else {
      val player = newPlayer(team, pos.get)
      squares(pos.get).enter(player).waitFor()
    }

    (Ack, state)
  }

  def flagDropped(flagTeam: Team, homeTeam: Team) = serialized { state =>
    if (flagTeam == homeTeam) {
      // Flag dropped in its own home: repop later
      gamePlay.delaySynchronize(flagRepopTime) {
        popFlag(flagTeam)
      }
      (Ack, state)
    } else {
      // Flag dropped in enemy's home: captured
      (Ack, state.incCapturedFlags(homeTeam))
    }
  }

  private def popFlag(team: Team): Unit = serialized { state =>
    val pos = findFreeSquare(team match {
      case `teamA` => flagsAPositions
      case `teamB` => flagsBPositions
    })

    if (pos.isEmpty) {
      // Try again later
      gamePlay.delaySynchronize(timeUnit) {
        popFlag(team)
      }
    } else {
      // Pop the flag
      squares(pos.get).popFlag(team)
    }

    ((), state)
  }

  private def findFreeSquare(candidates: List[Pos]): Option[Pos] = {
    val freeSquares = candidates filter { pos =>
      val info = squares(pos).getInfo
      !info.hasPlayer && !info.hasFlag
    }

    if (freeSquares.isEmpty)
      None
    else
      Some(Random.rand(freeSquares))
  }

  def isTerminated = serialized { state =>
    (state.terminated, state)
  }

  def terminate = serialized { state =>
    ((), state.copy(terminated = true))
  }

  def getScores = serialized { state =>
    ((state.capturedFlagsByA, state.capturedFlagsByB), state)
  }

  def getWinner = serialized { state =>
    (state.winner, state)
  }
}
