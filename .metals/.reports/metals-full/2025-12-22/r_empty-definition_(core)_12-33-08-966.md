file://<WORKSPACE>/core/src/it/unibo/pslab/multiparty/Language.scala
empty definition using pc, found symbol in pc: 
semanticdb not found
empty definition using fallback
non-local guesses:
	 -it/unibo/pslab/peers/Peers.TieToMultiple#
	 -it/unibo/pslab/multiparty/MultiParty.TieToMultiple#
	 -TieToMultiple#
	 -scala/Predef.TieToMultiple#
offset: 648
uri: file://<WORKSPACE>/core/src/it/unibo/pslab/multiparty/Language.scala
text:
```scala
package it.unibo.pslab.multiparty

import scala.annotation.nowarn
import it.unibo.pslab.peers.Peers.*
import it.unibo.pslab.multiparty.MultiParty.*

object Language:
  @nowarn
  inline def on[P <: Peer](using lp: LocalPeer)[V](body: PeerScope[P] ?=> MultiParty[V] | V): MultiParty[V on P] =
    given ps: PeerScope[P] = new PeerScope[P] {}
    Free.liftF(MultiPartyGrammar.On[V, P](lp)(_ => body))

  inline def comm[From <: TieTo[To], To <: TieTo[From]](using
      lp: LocalPeer
  )[V](value: V on From): MultiParty[PlacedKind[From, To, V]] =
    Free.liftF(MultiPartyGrammar.Comm[V, From, To](lp)(value))

  inline def commPerPeer[From <: TieToM@@ultiple[To], To <: TieToSingle[From]](using
      lp: LocalPeer
  )[V](value: PerPeer[V] on From): MultiParty[V on To] =
    Free.liftF(MultiPartyGrammar.CommPerPeer[V, From, To](lp)(value))

  inline def forEachPeer[From <: TieToMultiple[To], To <: Peer](using
      lp: LocalPeer
  )[V](body: PartialFunction[Remote[To], V]): MultiParty[PerPeer[V]] =
    Free.liftF(MultiPartyGrammar.ForEachPeer[V, From, To](lp)(body))

  inline def asLocal[P <: Peer: PeerScope](using lp: LocalPeer)[V](placed: V on P): MultiParty[V] =
    Free.liftF(MultiPartyGrammar.AsLocal[V, P](lp)(placed))

  @nowarn
  inline def asLocalAll[P <: Peer: PeerScope](using
      lp: LocalPeer
  )[V](placed: Many[V] on P): MultiParty[Map[Remote[?], V]] =
    Free.liftF(MultiPartyGrammar.AsLocalAll[V, P](lp)(placed))

  inline def remotes[RP <: Peer](using lp: LocalPeer)[L <: Peer: PeerScope]: MultiParty[Iterable[Remote[RP]]] =
    Free.liftF(MultiPartyGrammar.Remotes[RP, L](lp))
```


#### Short summary: 

empty definition using pc, found symbol in pc: 