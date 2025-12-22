package it.unibo.pslab.peers

object Peers:
  sealed trait Peer
  enum TieTo[P <: Peer] extends Peer:
    case TieToSingle[K <: Peer]() extends TieTo[K]
    case TieToMultiple[K <: Peer]() extends TieTo[K]
