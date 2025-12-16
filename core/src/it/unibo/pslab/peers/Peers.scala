package it.unibo.pslab.peers

object Peers:
  type Peer = { type Tie }
  
  type TieToSingle[P <: Peer] = { type Tie <: Multiplicity.Single[P] }
  
  type TieToMultiple[P <: Peer] = { type Tie <: Multiplicity.Multiple[P] }
  
  type TieTo[P <: Peer] = { type Tie <: Multiplicity[P] }
  
  enum Multiplicity[+P <: Peer]:
    case Single()
    case Multiple()
