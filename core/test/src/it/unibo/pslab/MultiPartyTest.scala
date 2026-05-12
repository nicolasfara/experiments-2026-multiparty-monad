package it.unibo.pslab

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should
import it.unibo.pslab.ScalaTropy.*
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.AnyProtocol
import it.unibo.pslab.peers.Peers.*
import it.unibo.pslab.network.*
import cats.Id
import it.unibo.pslab.multiparty.Environment.Reference
import cats.data.NonEmptyList
import cats.Monad
import cats.syntax.all.*

type PeerRef[P <: Peer] = String
class TestNetwork[P <: Peer] extends Network[Id, P, PeerRef] {
  override val local: PeerRef[P] = "local"

  override def send[V: Encodable[Id], To <: Peer: PeerTag](value: V, resource: Reference, to: PeerRef[To]): Id[Unit] = ???

  override def receive[V: Decodable[Id], From <: Peer: PeerTag](resource: Reference, from: PeerRef[From]): Id[V] = ???

  override def alivePeersOf[RP <: Peer: PeerTag]: Id[NonEmptyList[PeerRef[RP]]] = ???
}  

class MultiPartyTest extends AnyFunSpec, should.Matchers:
  type Foo <: { type Tie <: via[AnyProtocol toMultiple Foo] }
  type Bar <: Foo
  type Baz <: { type Tie <: via[AnyProtocol toMultiple Foo] }

  describe("The on operator"):
    describe("when involves a peer class with subtypes"):
      it("should evaluate the expression even in the subtypes classes"):
        def testProgram[F[_]: Monad](using MultiParty[F]) =
          var counter = 0
          for
            _ <- on[Foo] { (counter += 1).pure } // Evaluated
            _ <- on[Bar] { (counter += 1).pure } // Evaluated, since Bar <: Foo
            _ <- on[Baz] { (counter += 1).pure } // Not evaluated, since Baz is not a subtype of Foo or Bar
          yield 
            counter shouldBe 2
            ()

        ScalaTropy(testProgram[Id]).projectedOn[Bar]:
          tiedTo[Foo] via TestNetwork[Bar]()
