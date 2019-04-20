package fsm

import akka.actor.FSM
import fsm.AuctionFSM._

class AuctionFSM extends FSM[State, Data]{
  startWith(State.Start, Data.Empty)
}

object AuctionFSM {

  sealed trait State

  object State {

    case object Start extends State

  }

  sealed trait Data

  object Data {

    case object Empty extends Data

  }

}
