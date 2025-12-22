error id: file://<WORKSPACE>/examples/src/it/unibo/pslab/MainWorker.scala:it/unibo/pslab/multiparty/MultiParty.MultiParty#
file://<WORKSPACE>/examples/src/it/unibo/pslab/MainWorker.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb

found definition using fallback; symbol MultiParty
offset: 316
uri: file://<WORKSPACE>/examples/src/it/unibo/pslab/MainWorker.scala
text:
```scala
package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.TieTo.*

object MainWorker:
  type Main <: TieToMultiple[Worker]
  type Worker <: TieToSingle[Main]

  private case class Task(x: Int):
    def compute: Int = x * x

  def mainWorkerProgram(using LocalPeer): Mult@@iParty[Unit] = for
    task <- on[Main]:
      for
        peers <- remotes[Worker]
        allocation = peers.map(_ -> Task(scala.util.Random.nextInt(100))).toMap
        message <- forEachPeer[Main, Worker](allocation)
      yield message
    taskOnWorker <- commPerPeer[Main, Worker](task)
    _ <- on[Worker]:
      for t <- asLocal(taskOnWorker)
      yield println(s"Worker received task with input ${t.x}, computed result: ${t.compute}")
  yield ()

```


#### Short summary: 

empty definition using pc, found symbol in pc: 