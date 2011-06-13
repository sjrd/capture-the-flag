package util

import scala.ozma._
import ozma._

trait AdvancedStatelessPortObject {
  private val port = ResultPort.newStatelessPortObject {
    procedure: (() => Any) => procedure()
  }

  protected def serialized[A](procedure: => A): A = {
    port.send(() => procedure).asInstanceOf[A]
  }

  protected def delayedSerialized(ms: Int)(procedure: => Unit) {
    thread {
      sleep(ms)
      serialized(procedure)
    }
  }
}
