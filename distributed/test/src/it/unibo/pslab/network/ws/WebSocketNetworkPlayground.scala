package it.unibo.pslab.network.ws

import scala.concurrent.duration.DurationInt

import it.unibo.pslab.network.{ AnyProtocol, PeerId }
import it.unibo.pslab.peers.Peers.{ toSingle, via }
import it.unibo.pslab.network.ws.UpickleCodable.given

import cats.effect.{ IO, IOApp }
import it.unibo.pslab.multiparty.Environment
import it.unibo.pslab.peers.Peers.PeerTag

type Pinger <: { type Tie <: via[AnyProtocol toSingle Ponger] }
type Ponger <: { type Tie <: via[AnyProtocol toSingle Pinger] }

object WebSocketNetworkPlayground extends IOApp.Simple:

  override def run: IO[Unit] =
    val pinger = PeerId[Pinger]("ws://127.0.0.1:10000/ws")
    val ponger = PeerId[Ponger]("ws://127.0.0.1:10001/ws")
    for
      wsNet1 <- WebSocketNetwork.make[IO, Pinger](
        ipv4Address = "127.0.0.1",
        port = 10_000,
        knownPeers = Set(ponger),
      )
      _ <- IO.println(s"wsNet1 local peer: ${wsNet1.local}")
      wsNet2 <- WebSocketNetwork.make[IO, Ponger](
        ipv4Address = "127.0.0.1",
        port = 10_001,
        knownPeers = Set(pinger),
      )
      ref <- Environment.make[IO].provide(summon[PeerTag[Pinger]])
      ref2 <- Environment.make[IO].provide(summon[PeerTag[Ponger]])
      _ <- IO.println(s"wsNet2 local peer: ${wsNet2.local}")
      _ <- IO.sleep(1.seconds) // wait for servers to start and discover each other
      _ <- wsNet1.send[String, Ponger]("hello", ref, ponger)
      _ <- IO.sleep(5.seconds)
      v <- wsNet2.receive[String, Pinger](ref, pinger)
      _ <- IO.println(s"wsNet2 received: $v")
      _ <- wsNet2.send[String, Pinger]("world", ref2, pinger)
      v2 <- wsNet1.receive[String, Ponger](ref2, ponger)
      _ <- IO.println(s"wsNet1 received: $v2")
    yield ()
    end for
  end run
end WebSocketNetworkPlayground
