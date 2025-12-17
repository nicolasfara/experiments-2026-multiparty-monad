package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.Multiplicity.*

object Multiparty:
  type Pinger <: { type Tie <: Single[Ponger] }
  type Ponger <: { type Tie <: Single[Pinger] }

  def pingPong(initial: Int): MultiParty[Unit] = for
    pingMsg <- placed[Int, Pinger](initial)
    onPonger <- unicast[Int, Pinger, Ponger](pingMsg)
    _ <- placed[Unit, Ponger]:
      for
        msg <- await(onPonger)
        msg1 = msg + 1
        _ <- pingPong(msg1)
      yield ()
  yield ()
