package it.unibo.pslab.multiparty

import cats.syntax.all.*
import it.unibo.pslab.peers.Peers.Peer
import it.unibo.pslab.peers.Peers.*
import it.unibo.pslab.network.Network
import it.unibo.pslab.peers.Peers.PeerTag
import it.unibo.pslab.multiparty.Environment.Resource
import MultiParty.on
import cats.Monad

trait Label[+V]

trait Many[+V]

trait MultiParty[F[_]]:
  def on[Local <: Peer](using PeerTag[Local])[V](body: Label[Local] ?=> F[V]): F[V on Local]
  def comm[From <: TieSingle[To], To <: Peer](using from: PeerTag[From], to: PeerTag[To])[V](value: V on From): F[V on To]
  def take[Local <: Peer](using Label[Local])[V](placed: V on Local): F[V]

object MultiParty:
  opaque infix type on[+V, -P <: Peer] = Placement[V, P]

  private enum Placement[+V, -P <: Peer](val res: Resource):
    case Local(override val res: Resource, value: V) extends Placement[V, P](res)
    case Remote(override val res: Resource) extends Placement[V, P](res)

  def apply[F[_]: MultiParty as mp]: MultiParty[F] = mp

  def project[F[_]: Monad, P <: Peer: PeerTag](env: Environment[F], network: Network[F]): MultiParty[F] = new MultiParty[F]:
    def on[Local <: Peer](using local: PeerTag[Local])[V](body: Label[Local] ?=> F[V]): F[V on Local] =
      val p = summon[PeerTag[P]]
      given Label[Local] = new Label[Local] {}
      val resourceF = env.provide(local)
      if p == local then
        val result = body
        val valueF = result
        for
          resource <- resourceF
          value <- valueF
          _ <- network.send(value, resource)
        yield Placement.Local[V, Local](resource, value)
      else
        resourceF.map(res => Placement.Remote[V, Local](res))

    def comm[From <: TieSingle[To], To <: Peer](using from: PeerTag[From], to: PeerTag[To])[V](value: V on From): F[V on To] =
      val p = summon[PeerTag[P]]
      if p == from then
        val Placement.Local(res, v) = value.runtimeChecked
        network.send(v, res).map(_ => Placement.Remote[V, To](res))
      else if p == to then
        val Placement.Remote(res) = value.runtimeChecked
        network.receive[V](res).map(Placement.Local[V, To](res, _))
      else
        Monad[F].pure(Placement.Remote[V, To](value.res))

    def take[Local <: Peer](using Label[Local])[V](placed: V on Local): F[V] =
      val Placement.Local(res, v) = placed.runtimeChecked
      Monad[F].pure(v)
