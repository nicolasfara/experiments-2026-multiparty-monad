package it.unibo.pslab

import it.unibo.pslab.multiparty.{ Environment, MultiParty }
import it.unibo.pslab.network.Network
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.{ Monad, Parallel }
import cats.effect.kernel.{ MonadCancel, Resource }
import cats.syntax.parallel.*

object ScalaTropy:

  opaque type ScalaTropy[F[_], Result] = Impl[F, Result]

  def apply[F[_]: Monad, Result](program: MultiParty[F] ?=> F[Result]): ScalaTropy[F, Result] = Impl(program)

  extension [F[_], Result](trope: ScalaTropy[F, Result])
    def projectedOn[Local <: Peer: PeerTag](using
        networkRes: Resource[F, Network[F, Local]],
    )(using MonadCancel[F, Throwable]): F[Result] =
      networkRes.use(projection)

    def projectedOn[Local <: Peer: PeerTag](using
        network: Network[F, Local],
    )(using Monad[F]): F[Result] =
      projection[Local](network)

    def projectedOnMultiple[Local <: Peer: PeerTag](using
        networks: List[Network[F, Local]],
    )(using Monad[F], Parallel[F]): F[List[Result]] =
      networks.parTraverse(projection)

    private inline def projection[Local <: Peer: PeerTag](network: Network[F, Local])(using Monad[F]): F[Result] =
      val env = Environment.make[F]
      given MultiParty[F] = MultiParty.make(env, network)
      trope.program

  private class Impl[F[_]: Monad, Result](val program: MultiParty[F] ?=> F[Result])
