package it.unibo.pslab

import cats.effect.kernel.Sync
import cats.effect.std.MapRef
import cats.syntax.all.*

trait KeyValueStore[F[_], Key, Value]:
  def get(key: Key): F[Option[Value]]
  def put(key: Key, value: Value): F[Unit]

object KeyValueStore:
  def inMemory[F[_]: Sync, Key, Value]: F[KeyValueStore[F, Key, Value]] = MapRef
    .ofConcurrentHashMap[F, Key, Value]()
    .map: mapRef =>
      new KeyValueStore[F, Key, Value]:
        override def get(key: Key): F[Option[Value]] = mapRef(key).get
        override def put(key: Key, value: Value): F[Unit] = mapRef.setKeyValue(key, value)
