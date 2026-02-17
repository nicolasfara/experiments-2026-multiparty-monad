package it.unibo.pslab

import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.NetworkMonitor.withCsvMonitoring
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.Quantifier.*

import cats.{ Monad, MonadThrow }
import cats.data.NonEmptyList
import cats.effect.{ ExitCode, IO, IOApp }
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

import InefficientMainWorkerMatrix.*

object InefficientMainWorkerMatrix:
  type Main <: { type Tie <: Multiple[Worker] }
  type Worker <: { type Tie <: Single[Main] }

  case class PartialResult(chunk: VecChunk[Double]) derives ReadWriter

  case class Task(chunk: MatrixChunk[Double], vector: Vec[Double]) derives ReadWriter:
    def compute: PartialResult =
      val res = Vec(
        chunk.rows.values.slice(chunk.startRow, chunk.endRow).map(_.zip(vector.values).map { case (a, b) => a * b }.sum),
      )
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
            worker -> Task(MatrixChunk(startRow, endRow, matrix), vector)
          .toList
          .toMap
          .pure

object InefficientMatrixMain extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    args.headOption match
      case Some(label) =>
        withCsvMonitoring(s"evaluation/broadcasting-experiment-$label.csv"):
          val mqttNetwork = MqttNetwork.localBroker[IO, Main](Configuration(appId = "broadcast-master-worker-matrix"))
          ScalaTropy(mainWorkerApp[IO]).projectedOn[Main](using mqttNetwork).as(ExitCode.Success)
      case None => IO.println("Usage: InefficientMatrixMain <label>").as(ExitCode.Error)

object InefficientMatrixWorker extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "broadcast-master-worker-matrix"))
    ScalaTropy(mainWorkerApp[IO]).projectedOn[Worker](using mqttNetwork)
