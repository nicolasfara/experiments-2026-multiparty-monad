error id: file://<WORKSPACE>/examples/src/it/unibo/pslab/PingPong.scala:it/unibo/pslab/multiparty/MultiParty.MultiParty#
file://<WORKSPACE>/examples/src/it/unibo/pslab/PingPong.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb

found definition using fallback; symbol MultiParty
offset: 264
uri: file://<WORKSPACE>/examples/src/it/unibo/pslab/PingPong.scala
text:
```scala
package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.TieTo.TieToSingle

object Multiparty:
  type Pinger <: TieToSingle[Ponger]
  type Ponger <: TieToSingle[Pinger]

  def pingPongProgram(using LocalPeer): MultiPa@@rty[Unit] = for
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

```


#### Short summary: 

empty definition using pc, found symbol in pc: 