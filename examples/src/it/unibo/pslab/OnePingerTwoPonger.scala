package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.TieTo.TieToSingle
import it.unibo.pslab.multiparty.Language.*

object TrianglePingPong:
  type Alice <: TieToSingle[Bob] & TieToSingle[Andromeda]
  type Bob <: TieToSingle[Alice] & TieToSingle[Andromeda]
  type Andromeda <: TieToSingle[Bob] & TieToSingle[Alice]

  def pingPongProgram(using LocalPeer): MultiParty[Unit] = for
    initial <- on[Alice](0)
    _ <- pingPong(initial)
  yield ()

  def pingPong(initial: Int on Alice)(using LocalPeer): MultiParty[Unit] = for
    aliceSendToBob <- comm[Alice, Bob](initial)
    prepareMessageToAndromeda <- on[Bob]:
      asLocal(aliceSendToBob).map(_ + 1)
    bobSendToAndromeda <- comm[Bob, Andromeda](prepareMessageToAndromeda)
    prepareMessageToAlice <- on[Andromeda]:
      asLocal(bobSendToAndromeda).map(_ + 1)
    pingerSum <- comm[Andromeda, Alice](prepareMessageToAlice)
  yield pingPong(pingerSum)

@main
def mainDoublePonger(): Unit =
  println("Multiparty Ping-Pong example defined.")
  given LocalPeer = new LocalPeer {}
  val program = TrianglePingPong.pingPongProgram
  println(program)
