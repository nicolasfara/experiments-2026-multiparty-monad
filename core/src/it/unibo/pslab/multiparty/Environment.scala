package it.unibo.pslab.multiparty

import it.unibo.pslab.peers.Peers.PeerTag
import it.unibo.pslab.multiparty.Environment.Resource
import cats.Monad

// trait Counter[F[_]]:
//   def next: F[Int]

// object Counter:
//   def make[F[_]: Monad]: Counter[[V] =>> StateT[F, Int, V]] = new Counter[[V] =>> StateT[F, Int, V]]:
//     def next: StateT[F, Int, Int] = StateT { current =>
//       Monad[F].pure((current + 1, current))
//     }

trait Environment[F[_]]:
  def provide(peerTag: PeerTag[?]): F[Resource]

object Environment:
  trait Resource

  private case class ResourceImpl(id: Int, peerTag: PeerTag[?]) extends Resource

  def make[F[_]: Monad]: Environment[F] = new Environment[F]:
    private var counter = 0
    def provide(peerTag: PeerTag[?]): F[Resource] = Monad[F].pure:
      counter += 1
      ResourceImpl(counter, peerTag)
