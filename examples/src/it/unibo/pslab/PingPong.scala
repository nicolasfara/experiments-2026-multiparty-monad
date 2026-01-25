package it.unibo.pslab

import cats.syntax.all.*
import cats.effect.std.Console
import it.unibo.pslab.peers.Peers.*
import it.unibo.pslab.peers.Peers.Quantifier.*
import cats.Monad
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.multiparty.Environment
// import cats.Id
import cats.effect.IOApp
import cats.effect.IO
import it.unibo.pslab.network.mqtt.MqttNetwork
import cats.effect.kernel.Temporal
import scala.concurrent.duration.DurationInt
import it.unibo.pslab.network.Codable
import it.unibo.pslab.PingPong.*

object PingPong:
  type Pinger <: { type Tie <: Single[Ponger] }
  type Ponger <: { type Tie <: Single[Pinger] }

  def pingPongProgram[F[_]: {Monad, Console, Temporal}](using
      lang: MultiParty[F],
      codable: Codable[F, Int, lang.Format],
  ): F[Unit] = for
    initial <- on[Pinger](0.pure)
    _ <- pingPong(initial)
  yield ()

  def pingPong[F[_]: {Monad, Console, Temporal}](using
      lang: MultiParty[F],
      codable: Codable[F, Int, lang.Format],
  )(initial: Int on Pinger): F[Unit] =
    for
      onPonger <- comm[Pinger, Ponger](initial)
      newCounter <- on[Ponger]:
        for
          v <- take(onPonger)
          _ <- Console[F].println(s"Ponger received value: $v")
        yield v + 1
      newCounterOnPinger <- comm[Ponger, Pinger](newCounter)
      result <- on[Pinger]:
        for
          v <- take(newCounterOnPinger)
          _ <- Console[F].println(s"Pinger received value: $v")
        yield v + 1
      _ <- Temporal[F].sleep(1.second)
      _ <- pingPong(result)
    yield ()

// @main
// def main(): Unit =
//   val env = Environment.make[Id]
//   val network: Network[Id] = InMemoryNetwork.make[Id]
//   val lang = MultiParty.project[Id, Multiparty.Pinger](env, network)
//   val program = Multiparty.pingPongProgram[Id](using summon[Monad[Id]], summon[Console[Id]], lang)
//   program

given Codable[IO, Int, Array[Byte]] = new Codable[IO, Int, Array[Byte]]:
  override def encode(value: Int): IO[Array[Byte]] = IO.pure(value.toString.getBytes)
  override def decode(data: Array[Byte]): IO[Int] = IO.pure(new String(data).toInt)

object Pinger extends IOApp.Simple:
  override def run: IO[Unit] = MqttNetwork
    .localBroker[IO, PingPong.Pinger]("pinger-peer")
    .use: network =>
      val env = Environment.make[IO]
      val lang = MultiParty.make(env, network)
      val program = pingPongProgram[IO](using summon[Monad[IO]], summon[Console[IO]], summon[Temporal[IO]], lang)
      program

object Ponger extends IOApp.Simple:
  override def run: IO[Unit] = MqttNetwork
    .localBroker[IO, PingPong.Ponger]("ponger-peer")
    .use: network =>
      val env = Environment.make[IO]
      val lang = MultiParty.make(env, network)
      val program = pingPongProgram[IO](using summon[Monad[IO]], summon[Console[IO]], summon[Temporal[IO]], lang)
      program
