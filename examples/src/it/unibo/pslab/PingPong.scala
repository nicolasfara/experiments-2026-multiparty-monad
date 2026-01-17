package it.unibo.pslab

import cats.syntax.all.*
import it.unibo.pslab.peers.Peers.TieTo.TieToSingle
import cats.Monad
// import it.unibo.pslab.multiparty.MultiPartyLanguage.*
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.on
import it.unibo.pslab.multiparty.Environment
import cats.Id
import it.unibo.pslab.network.Network
import it.unibo.pslab.network.InMemoryNetwork

object Multiparty:
  type Pinger <: TieToSingle[Ponger]
  type Ponger <: TieToSingle[Pinger]

  def pingPongProgram[F[_]: Monad](l: MultiParty[F]): F[Unit] = for
    initial <- l.on[Pinger](0)
    _ <- pingPong(l)(initial)
  yield ()

  def pingPong[F[_]: Monad](l: MultiParty[F])(initial: Int on Pinger): F[Unit] = for
    onPonger <- l.comm[Pinger, Ponger](initial)
    newCounter <- l.on[Ponger]:
      l.take(onPonger).map(_ + 1)
    newCounterOnPinger <- l.comm[Ponger, Pinger](newCounter)
  yield () //pingPong(l)(newCounterOnPinger)

  // def pingPongProgram[Local <: Peer : LocalPeer]: MultiParty[Unit] = for
  //   initial <- on[Pinger](0)
  //   _ <- pingPong(initial)
  // yield ()

  // def pingPong[Local <: Peer : LocalPeer](initial: Int on Pinger): MultiParty[Unit] = for
  //   onPonger <- comm[Pinger, Ponger](initial)
  //   newCounter <- on[Ponger]:
  //     asLocal(onPonger).map(_ + 1)
  //   newCounterOnPinger <- comm[Ponger, Pinger](newCounter)
  // yield pingPong(newCounterOnPinger)

@main
def main(): Unit =
  val env = Environment.make[Id]
  val network: Network[Id] = InMemoryNetwork.make[Id]
  val lang = MultiParty.project[Id, Multiparty.Pinger](env, network)
  val program = Multiparty.pingPongProgram[Id](lang)
  program