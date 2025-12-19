package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.{MultiParty, await, comm, on, placed}
import it.unibo.pslab.peers.Peers.TieTo.TieToSingle

object TrianglePingPong:
  type Alice <: TieToSingle[Bob] & TieToSingle[Andromeda]
  type Bob <: TieToSingle[Alice] & TieToSingle[Andromeda]
  type Andromeda <: TieToSingle[Bob] & TieToSingle[Alice]
  def pingPongProgram: MultiParty[Unit] = for
    initial <- placed[Alice](0)
    _ <- pingPong(initial)
  yield ()

  def pingPong(initial: Int on Alice): MultiParty[Unit] = for
    aliceSendToBob <- comm[Alice, Bob](initial)
    prepareMessageToAndromeda <- placed[Bob]:
      await(aliceSendToBob).map(_ + 1)
    bobSendToAndromeda <- comm[Bob, Andromeda](prepareMessageToAndromeda)
    prepareMessageToAlice <- placed[Andromeda]:
      await(bobSendToAndromeda).map(_ + 1)
    pingerSum <- comm[Andromeda, Alice](prepareMessageToAlice)
  yield pingPong(pingerSum)

@main
def mainDoublePonger(): Unit =
  println("Multiparty Ping-Pong example defined.")
  val program = TrianglePingPong.pingPongProgram
  println(program)
