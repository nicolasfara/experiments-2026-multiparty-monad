package it.unibo.pslab.peers

object Peers:
  sealed trait Multiplicity

  object Multiplicity:
    sealed trait Single extends Multiplicity
    sealed trait Multiple extends Multiplicity

  trait Peer
  sealed trait TieToSingle[P <: Peer] extends Peer
  sealed trait TieToMultiple[P <: Peer] extends Peer
  type TieTo[P <: Peer] = TieToSingle[P] | TieToMultiple[P]
