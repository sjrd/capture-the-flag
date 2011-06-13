package util

import scala.ozma._
import ozma._

trait AdvancedPortObject {
  type State
  type StateTransition[+A] = State => (A, State)

  protected def initialState: State

  private val port = ResultPort.newPortObject(initialState) {
    (state: State, transition: StateTransition[Any]) => transition(state)
  }

  protected def serialized[A](transition: StateTransition[A]): A = {
    port.send(transition).asInstanceOf[A]
  }

  protected def delayedSerialized(ms: Int)(transition: State => State) {
    thread {
      sleep(ms)
      serialized { state =>
        ((), transition(state))
      }
    }
  }
}
