error id: file://<WORKSPACE>/core/src/it/unibo/pslab/multiparty/Algebra.scala:it/unibo/pslab/multiparty/MultiParty.PerPeer#
file://<WORKSPACE>/core/src/it/unibo/pslab/multiparty/Algebra.scala
empty definition using pc, found symbol in pc: 
found definition using semanticdb; symbol it/unibo/pslab/multiparty/MultiParty.PerPeer#
empty definition using fallback
non-local guesses:

offset: 551
uri: file://<WORKSPACE>/core/src/it/unibo/pslab/multiparty/Algebra.scala
text:
```scala
package it.unibo.pslab.multiparty

import it.unibo.pslab.peers.Peers.Peer
import it.unibo.pslab.peers.Peers.TieTo
import it.unibo.pslab.peers.Peers.TieTo.*
import it.unibo.pslab.multiparty.MultiParty.LocalPeer
import it.unibo.pslab.multiparty.MultiParty.PeerScope
import Language.{ MultiParty, on, PlacedKind}

enum MultiPartyGrammar[T]:
  case On[V, P <: Peer](ps: LocalPeer)(value: PeerScope[P] => MultiParty[V] | V) extends MultiPartyGrammar[V on P]
  case CommPerPeer[V, From <: TieToMultiple[To], To <: TieToSingle[From]](ps: LocalPeer)(value: Pe@@rPeer[V] on From)
      extends MultiPartyGrammar[V on To]
  case Comm[V, From <: TieTo[To], To <: TieTo[From]](ps: LocalPeer)(value: V on From)
      extends MultiPartyGrammar[PlacedKind[From, To, V]]
  case AsLocal[V, P <: Peer: PeerScope](ps: LocalPeer)(placed: V on P) extends MultiPartyGrammar[V]
  case AsLocalAll[V, P <: Peer: PeerScope](ps: LocalPeer)(placed: Many[V] on P)
      extends MultiPartyGrammar[Map[Remote[?], V]]
  case ForEachPeer[V, From <: TieToMultiple[To], To <: Peer](ps: LocalPeer)(value: PartialFunction[Remote[To], V])
      extends MultiPartyGrammar[PerPeer[V]]
  case Remotes[RP <: Peer, L <: Peer: PeerScope](ps: LocalPeer) extends MultiPartyGrammar[Iterable[Remote[RP]]]

```


#### Short summary: 

empty definition using pc, found symbol in pc: 