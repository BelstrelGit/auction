package fsm

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestFSMRef, TestKit}
import org.scalatest.{BeforeAndAfterAll, FreeSpecLike, Matchers}

class AuctionFSMSpec
  extends TestKit(ActorSystem("AuctionFSMSpec"))
    with FreeSpecLike
    with Matchers
    with ImplicitSender
    with BeforeAndAfterAll {

  import AuctionFSM.{State, Data}

  override def afterAll(): Unit = system.terminate()

  "AuctionFSM" - {
    "should has 'Start' state at start" in {
      val auc = TestFSMRef(new AuctionFSM)
      auc.stateName shouldBe State.Start
      auc.stateData shouldBe Data.Empty
    }
  }

}
