// package it.unibo.pslab.network.ws

// import it.unibo.pslab.network.PeerId

// import cats.effect.IO
// import cats.effect.kernel.Ref
// import cats.effect.std.Queue
// import cats.effect.testing.scalatest.AsyncIOSpec
// import org.http4s.websocket.WebSocketFrame
// import org.scalatest.freespec.AsyncFreeSpec
// import org.scalatest.matchers.should

// class PeerCoordinatorTest extends AsyncFreeSpec with AsyncIOSpec with should.Matchers {

//   "on peer discovery" - {
//     "adds new peer to alive peers" in {
//       for
//         impl <- makeImpl
//         _ <- impl.onPeerDiscovered(PeerId("peer1"))
//         peers <- impl.alivePeers.get
//       yield peers.keySet should contain(PeerId("peer1"))
//     }
//   }

//   def makeImpl: IO[PeerCoordinator.Impl[IO]] = Ref
//     .of[IO, Map[PeerId, Queue[IO, WebSocketFrame]]](Map.empty)
//     .map(PeerCoordinator.Impl("localhost", _))

// }
