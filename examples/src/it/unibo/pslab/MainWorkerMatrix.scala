package it.unibo.pslab

import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.Quantifier.*

import cats.effect.{ IO, IOApp }
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

import MainWorkerMatrix.*
import cats.MonadThrow
import scala.util.Random
import cats.data.NonEmptyList
import cats.Monad

case class Matrix[T](values: List[List[T]]) derives ReadWriter:
  val rows: Int = values.length
  val cols: Int = if rows > 0 then values.head.length else 0
  def rowSlice(start: Int, end: Int): Matrix[T] = Matrix(values.slice(start, end))
  def show: String = values.map(_.map(_.toString).mkString("  ")).mkString("\n")

object Matrix:
  def draw(size: Int = 50): Matrix[Double] = Matrix(
    List.fill(Random.nextInt(size) + 1, Random.nextInt(size) + 1)(Random.nextDouble() * 10),
  )

case class MatrixChunk[T](startRow: Int, endRow: Int, rows: Matrix[T]) derives ReadWriter

case class Vec[T](values: List[T]) derives ReadWriter:
  def show: String = values.map(_.toString).mkString("[", ", ", "]")

object Vec:
  def draw(size: Int): Vec[Double] = Vec(List.fill(size)(Random.nextDouble() * 10))

case class VecChunk[T](startRow: Int, endRow: Int, values: Vec[T]) derives ReadWriter

object MainWorkerMatrix:
  type Main <: { type Tie <: Multiple[Worker] }
  type Worker <: { type Tie <: Single[Main] }

  case class PartialResult(chunk: VecChunk[Double]) derives ReadWriter

  case class Task(chunk: MatrixChunk[Double], vector: Vec[Double]) derives ReadWriter:
    def compute: PartialResult =
      val res = Vec(chunk.rows.values.map(_.zip(vector.values).map { case (a, b) => a * b }.sum))
      PartialResult(VecChunk(chunk.startRow, chunk.endRow, res))

  object Task:
    def nil: Task = Task(MatrixChunk(0, 0, Matrix(Nil)), Vec(Nil))

  def mainWorkerApp[F[_]: {MonadThrow, Console}](using lang: MultiParty[F]): F[Unit] =
    for
      matrix <- on[Main](Matrix.draw(10).pure)
      vector <- on[Main](take(matrix).map(_.cols).map(Vec.draw))
      _ <- mainWorker(matrix, vector)
    yield ()

  def mainWorker[F[_]: {MonadThrow, Console}](using
      MultiParty[F],
  )(matrix: Matrix[Double] on Main, vector: Vec[Double] on Main): F[Unit] =
    for
      task <- on[Main]:
        for
          m <- take(matrix)
          v <- take(vector)
          _ <- F.println(s"Main has matrix:\n${m.show}\nand vector: ${v.show}")
          workers <- reachablePeers[Worker]
          allocation <- allocate(m, v) to workers
          message <- anisotropicMessage[Main, Worker](allocation, Task.nil)
        yield message
      taskOnWorker <- anisotropicComm[Main, Worker](task)
      partialResult <- on[Worker]:
        take(taskOnWorker).map(_.compute)
      allResults <- coAnisotropicComm[Worker, Main](partialResult)
      _ <- on[Main]:
        for
          resultsMap <- takeAll(allResults)
          result = resultsMap.values.toList.sortBy(_.chunk.startRow).flatMap(_.chunk.values.values)
          _ <- F.println(s"Main collected results from workers: ${Vec(result).show}")
        yield ()
    yield ()

  trait Allocator[F[_]]:
    infix def to(using l: MultiParty[F])(workers: NonEmptyList[l.Remote[Worker]]): F[Map[l.Remote[Worker], Task]]

  def allocate[F[_]: Monad, T](matrix: Matrix[Double], vector: Vec[Double]): Allocator[F] =
    new Allocator[F]:
      infix def to(using l: MultiParty[F])(workers: NonEmptyList[l.Remote[Worker]]): F[Map[l.Remote[Worker], Task]] =
        val chunkSize = math.ceil(matrix.rows.toDouble / workers.size).toInt
        workers
          .mapWithIndex: (worker, index) =>
            val startRow = index * chunkSize
            val endRow = math.min(startRow + chunkSize, matrix.rows)
            worker -> Task(MatrixChunk(startRow, endRow, matrix.rowSlice(startRow, endRow)), vector)
          .toList
          .toMap
          .pure

object MatrixMain extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Main](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Main](using mqttNetwork)

object MatrixWorker1 extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)

object MatrixWorker2 extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)
