package util

sealed class Ack {
  def waitFor() {
    // do nothing
  }
}

object Ack extends Ack
