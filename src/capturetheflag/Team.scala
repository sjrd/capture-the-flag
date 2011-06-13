package capturetheflag

class Team(val master: Master, val name: String, newBrain: Brain.NewBrainFun) {
  def newPlayer(pos: Pos) = {
    val brain = newBrain(this, pos, master.mapSize)
    new Player(this, brain, None, null)
  }
}
