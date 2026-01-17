package it.unibo.pslab.network

import it.unibo.pslab.multiparty.Environment.Resource
import cats.Applicative

trait Network[F[_]]:
  def send[V](value: V, resource: Resource): F[Unit]
  def receive[V](resource: Resource): F[V]
  def receiveAll[V](resource: Resource): F[Iterable[V]]

object InMemoryNetwork:
  def make[F[_]: Applicative]: Network[F] = new Network[F]:
    override def send[V](value: V, resource: Resource): F[Unit] = Applicative[F].unit
    override def receive[V](resource: Resource): F[V] = Applicative[F].pure(null.asInstanceOf[V])
    override def receiveAll[V](resource: Resource): F[Iterable[V]] = Applicative[F].pure(Iterable.empty)
