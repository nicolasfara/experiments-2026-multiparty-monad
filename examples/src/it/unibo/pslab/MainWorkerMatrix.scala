package it.unibo.pslab

import scala.util.Random

import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.NetworkMonitor.withCsvMonitoring
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.Quantifier.*

import cats.{ Monad, MonadThrow }
import cats.data.NonEmptyList
import cats.effect.{ IO, IOApp }
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

import MainWorkerMatrix.*

case class Matrix[T](values: List[List[T]]) derives ReadWriter:
  val rows: Int = values.length
  val cols: Int = if rows > 0 then values.head.length else 0
  def rowSlice(start: Int, end: Int): Matrix[T] = Matrix(values.slice(start, end))
  def show: String = values.map(_.map(_.toString).mkString("  ")).mkString("\n")

object Matrix:
  def draw(rows: Int = 10, cols: Int = 10): Matrix[Double] = Matrix(List.fill(rows, cols)(Random.nextDouble() * 10))

  def deterministic(rows: Int, cols: Int): Matrix[Double] =
    Matrix(List.tabulate(rows, cols)((i, j) => ((i + 1) * (j + 1)).toDouble))

case class MatrixChunk[T](startRow: Int, endRow: Int, rows: Matrix[T]) derives ReadWriter

case class Vec[T](values: List[T]) derives ReadWriter:
  def show: String = values.map(_.toString).mkString("[", ", ", "]")

object Vec:
  def draw(size: Int): Vec[Double] = Vec(List.fill(size)(Random.nextDouble() * 10))
  def deterministic(size: Int): Vec[Double] = Vec(List.tabulate(size)(i => (i + 1).toDouble))

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
      matrix <- on[Main](Matrix.deterministic(50, 50).pure)
      vector <- on[Main](take(matrix).map(_.cols).map(Vec.deterministic))
      _ <- impl(matrix, vector)
    yield ()

  def impl[F[_]: {MonadThrow, Console}](using
      MultiParty[F],
  )(matrix: Matrix[Double] on Main, vector: Vec[Double] on Main): F[Unit] =
    for
      tasks <- on[Main]:
        for
          m <- take(matrix)
          v <- take(vector)
          _ <- F.println(s"Main has matrix:\n${m.show}\nand vector: ${v.show}")
          workers <- reachablePeers[Worker]
          allocation <- allocate(m, v) to workers
          message <- anisotropicMessage[Main, Worker](allocation, Task.nil)
        yield message
      taskOnWorker <- anisotropicComm[Main, Worker](tasks)
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
  override def run: IO[Unit] = withCsvMonitoring("worker1-matrix.csv"):
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)

object MatrixWorker2 extends IOApp.Simple:
  override def run: IO[Unit] = withCsvMonitoring("worker2-matrix.csv"):
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)

object MatrixWorker3 extends IOApp.Simple:
  override def run: IO[Unit] = withCsvMonitoring("worker3-matrix.csv"):
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)

object MatrixWorker4 extends IOApp.Simple:
  override def run: IO[Unit] = withCsvMonitoring("worker4-matrix.csv"):
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)

object MatrixWorker5 extends IOApp.Simple:
  override def run: IO[Unit] = withCsvMonitoring("worker5-matrix.csv"):
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)

object MatrixWorker6 extends IOApp.Simple:
  override def run: IO[Unit] = withCsvMonitoring("worker6-matrix.csv"):
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)

object MatrixWorker7 extends IOApp.Simple:
  override def run: IO[Unit] = withCsvMonitoring("worker7-matrix.csv"):
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)

object MatrixWorker8 extends IOApp.Simple:
  override def run: IO[Unit] = withCsvMonitoring("worker8-matrix.csv"):
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)

object MatrixWorker9 extends IOApp.Simple:
  override def run: IO[Unit] = withCsvMonitoring("worker9-matrix.csv"):
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)

object MatrixWorker10 extends IOApp.Simple:
  override def run: IO[Unit] = withCsvMonitoring("worker10-matrix.csv"):
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "main-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)
