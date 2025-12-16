package build

import mill._
import scalalib._
import com.goyeau.mill.scalafix.ScalafixModule
import mill.contrib.scoverage.ScoverageModule
import $ivy.`com.lihaoyi::mill-contrib-scoverage:`
import $ivy.`com.goyeau::mill-scalafix::0.5.1`

trait CommonScala extends ScalaModule with ScoverageModule with ScalafixModule {
  def scalaVersion = "3.7.0"
  def scoverageVersion = "2.1.0"
  def scalacOptions = Seq(
    "-Werror",
    "-rewrite",
    "-indent",
    "-unchecked",
    "-explain",
    "-Xcheck-macros",
  )

  override def ivyDeps = Agg(
    ivy"com.softwaremill.ox::core:0.5.13",
    ivy"com.softwaremill.sttp.client4::ox:4.0.7",
    ivy"com.softwaremill.sttp.client4::circe:4.0.7",
    ivy"com.softwaremill.sttp.tapir::tapir-core:1.11.29",
    ivy"com.softwaremill.sttp.tapir::tapir-json-circe:1.11.29",
    ivy"com.softwaremill.sttp.tapir::tapir-netty-server-sync:1.11.29",
  )
}

object core extends CommonScala {
  object test extends ScalaTests with TestModule.ScalaTest {
    def testParallelism = true
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.19"
    )
  }
}

object examples extends CommonScala {
  def moduleDeps = Seq(core)
}
