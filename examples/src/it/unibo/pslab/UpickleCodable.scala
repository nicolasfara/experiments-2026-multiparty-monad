package it.unibo.pslab

import it.unibo.pslab.network.BinaryCodable

import cats.Applicative
import cats.syntax.all.*

object UpickleCodable:
  export upickle.default as upickle
  export upickle.*

  given [F[_]: Applicative, T: ReadWriter]: BinaryCodable[F, T] with
    override inline def encode(value: T): F[Array[Byte]] = writeBinary(value).pure[F]
    override inline def decode(data: Array[Byte]): F[T] = readBinary[T](data).pure[F]
