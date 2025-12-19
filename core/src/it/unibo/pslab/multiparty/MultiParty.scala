package it.unibo.pslab.multiparty

import cats.free.Free
import it.unibo.pslab.peers.Peers.{Peer, TieTo}
import it.unibo.pslab.peers.Peers.TieTo.*

import scala.annotation.nowarn

object MultiParty:
  infix opaque type on[+V, -P <: Peer] = Placement[V, P]

  private enum Placement[+V, -P <: Peer]:
    case Local(ref: String, value: V) extends Placement[V, P]
    case Remote(ref: String) extends Placement[V, P]

  trait PeerScope[P <: Peer]
  trait Many[+V]
  trait PerPeer[+V]
  trait Remote[-P <: Peer]

  type PlacedKind[From <: Peer, To <: Peer, V] = To match
    case TieToSingle[From]   => V on To
    case TieToMultiple[From] => Many[V] on To

  enum MultiPartyGrammar[T]:
    case Placed[V, P <: Peer](value: PeerScope[P] => MultiParty[V] | V) extends MultiPartyGrammar[V on P]
    case CommPerPeer[V, From <: TieToMultiple[To], To <: TieToSingle[From]](value: PerPeer[V] on From)
        extends MultiPartyGrammar[V on To]
    case Comm[V, From <: TieTo[To], To <: TieTo[From]](value: V on From)
        extends MultiPartyGrammar[PlacedKind[From, To, V]]
    case Await[V, P <: Peer: PeerScope](placed: V on P) extends MultiPartyGrammar[V]
    case AwaitAll[V, P <: Peer: PeerScope](placed: Many[V] on P) extends MultiPartyGrammar[Map[Remote[?], V]]
    case ForEachPeer[V, From <: TieToMultiple[To], To <: Peer](value: PartialFunction[Remote[To], V])
        extends MultiPartyGrammar[PerPeer[V]]
    case Remotes[RP <: Peer, L <: Peer: PeerScope]() extends MultiPartyGrammar[Iterable[Remote[RP]]]

  type MultiParty[T] = Free[MultiPartyGrammar, T]

  trait PlacedContinuation[P <: Peer]:
    def apply[V](body: PeerScope[P] ?=> MultiParty[V] | V): MultiParty[V on P] =
      given ps: PeerScope[P] = new PeerScope[P] {}
      Free.liftF(MultiPartyGrammar.Placed[V, P](_ => body))
  @nowarn
  inline def placed[P <: Peer]: PlacedContinuation[P] = new PlacedContinuation[P] {}

  trait CommContinuation[From <: TieTo[To], To <: TieTo[From]]:
    def apply[V](value: V on From): MultiParty[PlacedKind[From, To, V]] =
      Free.liftF(MultiPartyGrammar.Comm[V, From, To](value))
  @nowarn
  inline def comm[From <: TieTo[To], To <: TieTo[From]]: CommContinuation[From, To] = new CommContinuation[From, To] {}

  trait CommPerPeerContinuation[From <: TieToMultiple[To], To <: TieToSingle[From]]:
    def apply[V](value: PerPeer[V] on From): MultiParty[V on To] =
      Free.liftF(MultiPartyGrammar.CommPerPeer[V, From, To](value))

  @nowarn
  inline def commPerPeer[From <: TieToMultiple[To], To <: TieToSingle[From]]: CommPerPeerContinuation[From, To] =
    new CommPerPeerContinuation[From, To] {}

  trait ForEachPeerContinuation[From <: TieToMultiple[To], To <: Peer]:
    def apply[V](value: PartialFunction[Remote[To], V]): MultiParty[PerPeer[V]] =
      Free.liftF(MultiPartyGrammar.ForEachPeer[V, From, To](value))
  @nowarn
  inline def forEachPeer[From <: TieToMultiple[To], To <: Peer]: ForEachPeerContinuation[From, To] =
    new ForEachPeerContinuation[From, To] {}

  trait AwaitContinuation[P <: Peer]:
    def apply[V](placed: V on P)(using PeerScope[P]): MultiParty[V] =
      Free.liftF(MultiPartyGrammar.Await[V, P](placed))
  @nowarn
  inline def await[V, P <: Peer: PeerScope]: AwaitContinuation[P] = new AwaitContinuation[P] {}

  trait AwaitAllContinuation[P <: Peer]:
    def apply[V](placed: Many[V] on P)(using PeerScope[P]): MultiParty[Map[Remote[?], V]] =
      Free.liftF(MultiPartyGrammar.AwaitAll[V, P](placed))
  @nowarn
  inline def awaitAll[P <: Peer: PeerScope]: AwaitAllContinuation[P] = new AwaitAllContinuation[P] {}

  inline def remotes[RP <: Peer]()[L <: Peer](using PeerScope[L]): MultiParty[Iterable[Remote[RP]]] =
    Free.liftF(MultiPartyGrammar.Remotes[RP, L]())
