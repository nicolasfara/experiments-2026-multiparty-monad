package it.unibo.pslab.network

import cats.effect.kernel.Sync
import java.io.BufferedWriter
import cats.Applicative
import cats.effect.kernel.Resource
import java.io.FileWriter

trait NetworkMonitor[F[_]]:
  def onSend(payload: Array[Byte]): F[Unit]
  def onReceive(payload: Array[Byte]): F[Unit]

object NetworkMonitor:

  given noOp[F[_]: Applicative]: NetworkMonitor[F] = new NetworkMonitor[F]:
    override inline def onSend(payload: Array[Byte]): F[Unit] = Applicative[F].unit
    override inline def onReceive(payload: Array[Byte]): F[Unit] = Applicative[F].unit

  def withCsvMonitoring[F[_]: Sync, Result](path: String)(logic: NetworkMonitor[F] ?=> F[Result]): F[Result] =
    csv(path).use: monitor =>
      given NetworkMonitor[F] = monitor
      logic

  def csv[F[_]: Sync](path: String): Resource[F, NetworkMonitor[F]] = Resource
    .make(
      F.delay:
        val writer = new BufferedWriter(new FileWriter(path))
        writer.write("operation,payload_size\n")
        writer.flush()
        writer,
    )(writer => F.delay(writer.close()))
    .map: writer =>
      new NetworkMonitor[F]:
        override def onReceive(payload: Array[Byte]): F[Unit] = append("receive", payload)
        override def onSend(payload: Array[Byte]): F[Unit] = append("send", payload)
        private def append(operation: String, payload: Array[Byte]): F[Unit] = F.delay:
          val entry = s"$operation,${payload.length}\n"
          writer.write(entry)
          writer.flush()
