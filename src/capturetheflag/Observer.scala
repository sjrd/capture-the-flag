package capturetheflag

import scala.ozma._

class Observer(val master: Master) {
  val mapWidth = master.mapWidth
  val mapHeight = master.mapHeight

  val teamA = master.teamA
  val teamB = master.teamB

  val refreshDelay = master.timeUnit / 3

  def observe() {
    def loop() {
      val terminated = master.isTerminated
      clearScreen(25)
      observeGame()

      if (!terminated) {
        sleep(refreshDelay)
        loop()
      }
    }

    loop()
  }

  def clearScreen(count: Int) = {
    for (i <- 1 to count)
      println()
  }

  def observeGame() {
    for (y <- 0 to (mapHeight-1)) {
      for (x <- 0 to (mapWidth-1))
        printInfoAsChar(master.squares(Pos(x, y)).getInfo)
      println()
    }

    val (scoreA, scoreB) = master.getScores

    println()
    println("Team A: "+scoreA + "    Team B: "+scoreB)
  }

  def printInfoAsChar(info: SquareInfo) {
    print((info.player, info.flag) match {
      case (Some(`teamA`), None) => "a"
      case (Some(`teamB`), None) => "b"
      case (Some(`teamA`), Some(_)) => "A"
      case (Some(`teamB`), Some(_)) => "B"
      case (None, Some(`teamA`)) => "F"
      case (None, Some(`teamB`)) => "P"
      case (None, None) =>
        if (info.isHome) "@"
        else if (info.kind == SquareKind.Bomb) "X"
        else if (info.kind == SquareKind.Food) "*"
        else "."
    })
  }
}
