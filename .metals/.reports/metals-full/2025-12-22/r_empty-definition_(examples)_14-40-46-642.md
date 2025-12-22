error id: file://<WORKSPACE>/examples/src/it/unibo/pslab/OnePingerTwoPonger.scala:it/unibo/pslab/multiparty/MultiParty.MultiParty#
file://<WORKSPACE>/examples/src/it/unibo/pslab/OnePingerTwoPonger.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb

found definition using fallback; symbol MultiParty
offset: 368
uri: file://<WORKSPACE>/examples/src/it/unibo/pslab/OnePingerTwoPonger.scala
text:
```scala
package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.TieTo.TieToSingle

object TrianglePingPong:
  type Alice <: TieToSingle[Bob] & TieToSingle[Andromeda]
  type Bob <: TieToSingle[Alice] & TieToSingle[Andromeda]
  type Andromeda <: TieToSingle[Bob] & TieToSingle[Alice]

  def pingPongProgram(using LocalPeer): Multi@@Party[Unit] = for
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

```


#### Short summary: 

empty definition using pc, found symbol in pc: 