package it.unibo.pslab.multiparty

import cats.free.Free
import it.unibo.pslab.peers.Peers.{Peer, TieToMultiple, TieToSingle}

object MultiParty:
  infix opaque type on[+V, -P <: Peer] = Placement[V, P]

  trait Token[V, P <: Peer]

  private enum Placement[+V, -P <: Peer]:
    case Local(ref: String, value: V) extends Placement[V, P]
    case Remote(ref: String) extends Placement[V, P]

  trait PeerScope[P <: Peer]

  enum MultiPartyGrammar[T]:
    case Placed[P <: Peer, V](value: PeerScope[P] => MultiParty[V]) extends MultiPartyGrammar[V on P]
    case Par[A, PA <: Peer, B, PB <: Peer](left: PeerScope[PA] => MultiParty[A], right: PeerScope[PB] => MultiParty[B])
        extends MultiPartyGrammar[(A on PA, B on PB)]
    case Unicast[V, From <: Peer, To <: TieToSingle[From]](value: V on From) extends MultiPartyGrammar[V on To]
    case Multicast[V, From <: Peer, To <: TieToMultiple[From]](value: V on From) extends MultiPartyGrammar[V on To]
    case Await[V, Other <: Peer, P <: TieToSingle[Other]: PeerScope](placed: V on Other) extends MultiPartyGrammar[V]
    case AwaitAll[V, Other <: Peer, P <: TieToMultiple[Other]: PeerScope](placed: V on Other)
        extends MultiPartyGrammar[Iterable[V]]

  type MultiParty[T] = Free[MultiPartyGrammar, T]
