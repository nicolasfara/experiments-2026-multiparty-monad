#!/usr/bin/env -S scala-cli shebang --scala-version 3.8.1

import java.io.File
import java.nio.file.Path
import java.nio.file.{ Files, Paths, StandardCopyOption }
import java.util.Comparator.reverseOrder
import scala.sys.process.*
import scala.jdk.CollectionConverters.*
import scala.concurrent.duration.*
import scala.concurrent.{ Future, Await, ExecutionContext, TimeoutException }
import ExecutionContext.Implicits.global

object ExperimentRunner:

  private val basePackage = "it.unibo.pslab"
  private val evaluationDirectory = Paths.get("evaluation")
  private val classpath = "out/examples/assembly.dest/out.jar"
  private val maxWorkers = 32
  private var lastSuccessfulWorkers = 0
  private val experimentTimeout = 50.seconds

  def main(args: Array[String]): Unit =
    try
      prepare()
      compile()
      runExperimentsUsingBroadcastComm(maxWorkers)
      runExperimentsUsingSelectiveComm(lastSuccessfulWorkers)
      plot()
      logInfo(s"All done! Generated plots are available at ${evaluationDirectory.toAbsolutePath()}")
    catch case error: Exception => logError(error.getMessage())

  def prepare(): Unit =
    mkdir(evaluationDirectory)
    cleanup(evaluationDirectory)

  def compile(): Unit =
    val exitCode = "./mill examples.assembly".!
    if exitCode != 0 then throw RuntimeException(s"Compilation failed with exit code $exitCode")

  def runExperimentsUsingSelectiveComm(maxWorkers: Int): Unit =
    logInfo("Running MatMul Master-Worker program with selective style communications")
    runExperiments(
      maxWorkers,
      masterClass = s"$basePackage.matmul.MatMulMaster",
      workerClass = s"$basePackage.matmul.MatMulWorker",
      label = "selective experiment",
    )

  def runExperimentsUsingBroadcastComm(maxWorkers: Int): Unit =
    logInfo("Running MatMul Master-Worker program with broadcasting style communications")
    runExperiments(
      maxWorkers,
      masterClass = s"$basePackage.matmul.InefficientMatMulMaster",
      workerClass = s"$basePackage.matmul.InefficientMatMulWorker",
      label = "broadcasting (inefficient) experiment",
    )

  def runExperiments(maxWorkers: Int, masterClass: String, workerClass: String, label: String): Unit =
    var workers = 1
    var continueExperiments = true
    while workers <= maxWorkers && continueExperiments do
      logInfo(s"Starting $label with $workers worker(s)")
      val masterProcess = javaProcess(masterClass, s"$workers-workers").run()
      val workerProcesses = (1 to workers).map(i => javaProcess(workerClass, i.toString).run())
      val exitCodeFuture = Future(masterProcess.exitValue())
      try
        val exitCode = Await.result(exitCodeFuture, experimentTimeout)
        if exitCode != 0 then throw RuntimeException(s"$label with $workers worker(s) failed.")
        lastSuccessfulWorkers = workers
        workers *= 2
      catch
        case _: TimeoutException =>
          masterProcess.destroy()
          workerProcesses.foreach(_.destroy())
          continueExperiments = false
          removeMatching(evaluationDirectory, s".*${workers}(-workers)?\\.csv")
          logWarning(s"""
            |$label with $workers worker(s) timed out.
            |Probably you've run out of resources and your setup is not able to run $workers IOApp in parallel.
            |Plots will be limited to $lastSuccessfulWorkers worker(s).
           """.stripMargin.trim)

  def plot(): Unit =
    logInfo("Plotting results")
    ensurePythonEnvironment()
    val exitCode = Process(Seq(pythonExecutable, "plot_results.py")).!
    if exitCode != 0 then throw RuntimeException(s"Failed to plot results.")

  def ensurePythonEnvironment(): Unit =
    if !venvExists then
      createVenv()
      installPythonDependencies()

  def venvExists: Boolean =
    Files.exists(Paths.get(if isWindows then "venv\\Scripts\\python.exe" else "venv/bin/python"))

  def createVenv(): Unit =
    val exitCode = Process(Seq("python3", "-m", "venv", "venv")).!
    if exitCode != 0 then throw RuntimeException(s"Failed to create virtual environment.")

  def installPythonDependencies(): Unit =
    val pip = if isWindows then "venv\\Scripts\\pip.exe" else "venv/bin/pip"
    val exitCode = Process(Seq(pip, "install", "-r", "requirements.txt")).!
    if exitCode != 0 then throw RuntimeException(s"Failed to install Python dependencies.")

  def pythonExecutable: String = if isWindows then "venv\\Scripts\\python.exe" else "venv/bin/python"

  def isWindows: Boolean =
    System.getProperty("os.name").toLowerCase.contains("win")

  def cleanup(path: Path = currentPath): Unit =
    Files.walk(path).sorted(reverseOrder()).filter(_ != path).forEach(Files.delete)

  def mkdir(path: Path): Unit = if !Files.exists(path) then Files.createDirectories(path)

  def javaProcess(fqn: String, args: String*): ProcessBuilder =
    Process(Seq("java", "-cp", classpath, fqn) ++ args)

  def move(source: Path, target: Path): Unit = Files.move(source, target)

  def removeMatching(directory: Path, pattern: String): Unit =
    Files.list(directory).filter(p => p.getFileName.toString.matches(pattern)).forEach(Files.delete)

  def currentPath: Path = Paths.get(".").toAbsolutePath.normalize()

  def csvFiles(directory: Path = currentPath): Iterator[Path] =
    Files.list(directory).filter(p => p.toString.endsWith(".csv")).iterator().asScala

  def logInfo(message: String): Unit = println("=" * 120 + s"\n$message\n" + "=" * 120)

  def logWarning(message: String): Unit = println(s"\u001b[33mWARNING: $message\u001b[0m")

  def logError(message: String): Unit = System.err.println(s"\u001b[31mERROR: $message\u001b[0m")
end ExperimentRunner

ExperimentRunner.main(args)
