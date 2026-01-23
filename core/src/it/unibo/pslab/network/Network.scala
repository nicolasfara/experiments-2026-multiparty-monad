package it.unibo.pslab.network

import it.unibo.pslab.multiparty.Environment.Reference
import cats.Applicative
import it.unibo.pslab.peers.Peers.{Peer, PeerTag}

trait Network[F[_], LP <: Peer]:
  type Address[P <: Peer]

  val localAddress: Address[LP]

  def send[V, To <: Peer: PeerTag](value: V, resource: Reference, to: Address[To]): F[Unit]
  def receive[V, From <: Peer: PeerTag](resource: Reference, from: Address[From]): F[V]
  def receiveAll[V, From <: Peer: PeerTag, To <: Peer: PeerTag](resource: Reference, from: Address[From]): F[Map[Address[To], V]]
  def alivePeersOf[RP <: Peer: PeerTag]: F[Iterable[Address[RP]]]

object InMemoryNetwork:
  def make[F[_]: Applicative, LP <: Peer](using localAddress: PeerTag[LP]): Network[F, LP] = new Network[F, LP]:
    type Address[P <: Peer] = PeerTag[P]

    override val localAddress: Address[LP] = localAddress
    override def send[V, To <: Peer: PeerTag](value: V, resource: Reference, to: Address[To]): F[Unit] = Applicative[F].unit
    override def receive[V, From <: Peer: PeerTag](resource: Reference, from: Address[From]): F[V] = Applicative[F].pure(null.asInstanceOf[V])
    override def receiveAll[V, From <: Peer: PeerTag, To <: Peer: PeerTag](resource: Reference, from: Address[From]): F[Map[Address[To], V]] =
      Applicative[F].pure(Map.empty)

    override def alivePeersOf[RP <: Peer: PeerTag]: F[Iterable[Address[RP]]] = Applicative[F].pure(Iterable.empty)
