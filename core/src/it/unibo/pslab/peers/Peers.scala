package it.unibo.pslab.peers

import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.Type

object Peers:

  sealed trait PeerTag[-P <: Peer]

  private case class PeerTagImpl[-P <: Peer](fqn: String) extends PeerTag[P]

  inline given syntesizePeerTag[P <: Peer]: PeerTag[P] = ${ syntesizePeerTagImpl[P] }

  private def syntesizePeerTagImpl[P <: Peer: Type](using quotes: Quotes): Expr[PeerTag[P]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[P]
    val fqn = tpe.typeSymbol.fullName
    '{ PeerTagImpl[P](${ Expr(fqn) }) }

  sealed trait Peer

  enum TieTo[P <: Peer] extends Peer:
    case TieToSingle[K <: Peer]() extends TieTo[K]
    case TieToMultiple[K <: Peer]() extends TieTo[K]
