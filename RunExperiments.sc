#!/usr/bin/env scala

import java.io.File
import java.nio.file.Path
import scala.sys.process.*
import java.nio.file.{ Files, Paths, StandardCopyOption }
import java.util.Comparator.reverseOrder
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

object ExperimentRunner:

  private val basePackage = "it.unibo.pslab"
  private val evaluationDirectory = Paths.get("evaluation")
  private val classpath = "out/examples/assembly.dest/out.jar"
  private val maxWorkers = 32

  def main(args: Array[String]): Unit =
    prepare()
    compile()
    runExperimentsUsingBroadcastComm(maxWorkers)
    runExperimentsUsingSelectiveComm(maxWorkers)
    plot()

  def prepare(): Unit =
    mkdir(evaluationDirectory)
    cleanup(evaluationDirectory)

  def compile(): Unit = "./mill examples.assembly".!

  def runExperimentsUsingSelectiveComm(maxWorkers: Int): Unit =
    log("Running MatMul Master-Worker program with selective style communications")
    runExperiments(
      maxWorkers,
      masterClass = s"$basePackage.matmul.MatMulMaster",
      workerClass = s"$basePackage.matmul.MatMulWorker",
      label = "selective experiment",
    )

  def runExperimentsUsingBroadcastComm(maxWorkers: Int): Unit =
    log("Running MatMul Master-Worker program with broadcasting style communications")
    runExperiments(
      maxWorkers,
      masterClass = s"$basePackage.matmul.InefficientMatMulMaster",
      workerClass = s"$basePackage.matmul.InefficientMatMulWorker",
      label = "broadcasting (inefficient) experiment",
    )

  def runExperiments(maxWorkers: Int, masterClass: String, workerClass: String, label: String): Unit =
    LazyList
      .iterate(1)(_ * 2)
      .takeWhile(_ <= maxWorkers)
      .foreach: workers =>
        log(s"Starting $label with $workers worker(s)")
        val masterProcess = process(masterClass, s"$workers-workers").run()
        val workerProcesses = (1 to workers).map(i => process(workerClass, i.toString).run())
        val exitCode = masterProcess.exitValue()
        Thread.sleep(2_000)

  def plot(): Unit =
    log("Plotting results")
    ensurePythonEnvironment()
    Process(Seq(pythonExecutable, "plot_results.py")).!

  def ensurePythonEnvironment(): Unit =
    if !venvExists then
      createVenv()
      installPythonDependencies()

  def venvExists: Boolean =
    Files.exists(Paths.get(if isWindows then "venv\\Scripts\\python.exe" else "venv/bin/python"))

  def createVenv(): Unit = Process(Seq("python3", "-m", "venv", "venv")).!

  def installPythonDependencies(): Unit =
    val pip = if isWindows then "venv\\Scripts\\pip.exe" else "venv/bin/pip"
    Process(Seq(pip, "install", "-r", "requirements.txt")).!

  def pythonExecutable: String = if isWindows then "venv\\Scripts\\python.exe" else "venv/bin/python"

  def isWindows: Boolean =
    System.getProperty("os.name").toLowerCase.contains("win")

  def cleanup(path: Path = currentPath): Unit =
    Files.walk(path).sorted(reverseOrder()).filter(_ != path).forEach(Files.delete)

  def mkdir(path: Path): Unit = if !Files.exists(path) then Files.createDirectories(path)

  def process(fqn: String, args: String*): ProcessBuilder =
    Process(Seq("java", "-cp", classpath, fqn) ++ args)

  def move(source: Path, target: Path): Unit = Files.move(source, target)

  def currentPath: Path = Paths.get(".").toAbsolutePath.normalize()

  def csvFiles(directory: Path = currentPath): Iterator[Path] =
    Files.list(directory).filter(p => p.toString.endsWith(".csv")).iterator().asScala

  def log(message: String): Unit = println("=" * 120 + s"\n$message\n" + "=" * 120)
end ExperimentRunner

ExperimentRunner.main(args)
