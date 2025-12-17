package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.multiparty.MultiParty.MultiParty
import it.unibo.pslab.peers.Peers.Multiplicity.*

import java.util.UUID

object IdRequest:
  type IdProvider <: { type Tie <: Multiple[IdRequester] }
  type IdRequester <: { type Tie <: Single[IdProvider] }

  def idRequestProgram: MultiParty[Unit] = for
    cid <- placed[UUID, IdRequester](UUID.randomUUID())
    cidOnProvider <- unicast[UUID, IdRequester, IdProvider](cid)
    assignedId <- placed[(UUID, Int), IdProvider]:
      await(cidOnProvider).map(id => (id, id.hashCode().abs % 1000))
    assignedIdOnRequester <- multicast[(UUID, Int), IdProvider, IdRequester](assignedId)
    _ <- placed[Unit, IdRequester]:
      for 
        ids <- awaitAll(assignedIdOnRequester)
        id = ids.find(_._1 == cid).map(_._2)
      yield println(s"IdRequester received assigned ID: ${id.getOrElse(-1)}")
  yield ()
