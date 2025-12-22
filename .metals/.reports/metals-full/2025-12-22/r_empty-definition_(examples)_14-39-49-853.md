error id: file://<WORKSPACE>/examples/src/it/unibo/pslab/IdRequest.scala:it/unibo/pslab/multiparty/MultiParty.MultiParty#
file://<WORKSPACE>/examples/src/it/unibo/pslab/IdRequest.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb

found definition using fallback; symbol MultiParty
offset: 355
uri: file://<WORKSPACE>/examples/src/it/unibo/pslab/IdRequest.scala
text:
```scala
package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.TieTo.*

import java.util.UUID
import scala.util.Random

object IdRequest:
  type IdProvider <: TieToMultiple[IdRequester]
  type IdRequester <: TieToSingle[IdProvider]

  private def assignTask(requests: Map[Remote[?], UUID])(using LocalPeer): MultiP@@arty[PerPeer[Int]] =
    val assignments = requests.map
    forEachPeer[IdProvider, IdRequester](requests.map(_._1 -> Random.nextInt()))

  def idRequestProgram(using LocalPeer): MultiParty[Unit] = for
    cid <- on[IdRequester](UUID.randomUUID())
    cidOnProvider <- comm[IdRequester, IdProvider](cid)
    assignedId <- on[IdProvider]:
      for
        cidMap <- asLocalAll(cidOnProvider)
        tasks <- assignTask(cidMap)
      yield tasks
    assignedIdOnRequester <- commPerPeer[IdProvider, IdRequester](assignedId)
    _ <- on[IdRequester]:
      for id <- asLocal(assignedIdOnRequester)
      yield println(s"IdRequester received assigned ID: $id")
  yield ()

```


#### Short summary: 

empty definition using pc, found symbol in pc: 