package it.unibo.pslab

import cats.syntax.all.*
import it.unibo.pslab.peers.Peers.*
import it.unibo.pslab.peers.Peers.Quantifier.*
import cats.Monad
// import it.unibo.pslab.multiparty.MultiPartyLanguage.*
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.on
import it.unibo.pslab.multiparty.Environment
import cats.Id
import it.unibo.pslab.network.Network
import it.unibo.pslab.network.InMemoryNetwork
import cats.effect.IOApp
import cats.effect.IO
// import cats.Defer

object Multiparty:
  type Pinger <: { type Tie <: Single[Ponger] }
  type Ponger <: { type Tie <: Single[Pinger] }

  def pingPongProgram[F[_]: Monad](l: MultiParty[F]): F[Unit] = for
    initial <- l.on[Pinger](0.pure)
    _ <- pingPong(l)(initial)
  yield ()

  def pingPong[F[_]: Monad](l: MultiParty[F])(initial: Int on Pinger): F[Unit] = for
    onPonger <- l.comm[Pinger, Ponger](initial)
    newCounter <- l.on[Ponger]:
      l.take(onPonger).map(_ + 1)
    newCounterOnPinger <- l.comm[Ponger, Pinger](newCounter)
    _ <- pingPong(l)(newCounterOnPinger)
  yield ()

@main
def main(): Unit =
  val env = Environment.make[Id]
  val network: Network[Id] = InMemoryNetwork.make[Id]
  val lang = MultiParty.project[Id, Multiparty.Pinger](env, network)
  val program = Multiparty.pingPongProgram[Id](lang)
  program

object PingPongIOApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val env = Environment.make[IO]
    val network: Network[IO] = InMemoryNetwork.make[IO]
    val lang = MultiParty.project[IO, Multiparty.Pinger](env, network)
    val program = Multiparty.pingPongProgram[IO](lang)
    program