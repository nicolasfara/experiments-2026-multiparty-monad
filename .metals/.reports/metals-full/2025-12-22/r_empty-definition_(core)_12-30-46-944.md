error id: file://<WORKSPACE>/core/src/it/unibo/pslab/multiparty/Algebra.scala:
file://<WORKSPACE>/core/src/it/unibo/pslab/multiparty/Algebra.scala
empty definition using pc, found symbol in pc: 
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -Peer#
	 -scala/Predef.Peer#
offset: 83
uri: file://<WORKSPACE>/core/src/it/unibo/pslab/multiparty/Algebra.scala
text:
```scala
package it.unibo.pslab.multiparty

enum MultiPartyGrammar[T]:
  case On[V, P <: Pee@@r](ps: LocalPeer)(value: PeerScope[P] => MultiParty[V] | V) extends MultiPartyGrammar[V on P]
  case CommPerPeer[V, From <: TieToMultiple[To], To <: TieToSingle[From]](ps: LocalPeer)(value: PerPeer[V] on From)
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