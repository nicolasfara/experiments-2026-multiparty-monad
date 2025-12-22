package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.TieTo.TieToSingle
import it.unibo.pslab.multiparty.Language.*

object Multiparty:
  type Pinger <: TieToSingle[Ponger]
  type Ponger <: TieToSingle[Pinger]

  def pingPongProgram(using LocalPeer): MultiParty[Unit] = for
    initial <- on[Pinger](0)
    _ <- pingPong(initial)
  yield ()

  def pingPong(initial: Int on Pinger)(using LocalPeer): MultiParty[Unit] = for
    onPonger <- comm[Pinger, Ponger](initial)
    newCounter <- on[Ponger]:
      asLocal(onPonger).map(_ + 1)
    newCounterOnPinger <- comm[Ponger, Pinger](newCounter)
  yield pingPong(newCounterOnPinger)

//@main
//def main(): Unit =
//  println("Multiparty Ping-Pong example defined.")
//  val program = Multiparty.pingPongProgram
//  println(program)
