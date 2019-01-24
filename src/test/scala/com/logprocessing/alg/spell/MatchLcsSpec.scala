package com.logprocessing.alg.spell

import java.util.UUID
import com.logprocessing.log._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class MatchLcsSpec extends WordSpec {
  val logA = line("A")
  val logAB = line("AB")
  val logABC = line("ABC")
  val cls = clusters(logA, logAB, logABC)

  val spell = Spell(null, tau = 0.5)
  val ids: Seq[EventType] = (0 to 5).map(_ => UUID.randomUUID())

  "matchLcs" should {
    "handle basic cases correctly" in {
      spell.matchLcs(cls, seq("N")) shouldBe None
      spell.matchLcs(cls, IndexedSeq.empty).get shouldBe logA
      spell.matchLcs(cls, seq("XYZ")) shouldBe None
      spell.matchLcs(cls, seq("A")).get shouldBe logA
      spell.matchLcs(cls, seq("AB")).get shouldBe logAB
      spell.matchLcs(cls, seq("ABC")).get shouldBe logABC
    }

    "find the maximally matching cluster" in {
      val logA_CDEF = line("A*CDEF")
      val logAB_DEF = line("AB*DEF")
      val log_BC_E_ = line("*BC*E*")
      val log_BCDE_ = line("*BCDE*")
      val log_B__E_ = line("*B**E*")
      val cls = clusters(logA_CDEF, logAB_DEF, log_BC_E_, log_BCDE_, log_B__E_)

      spell.matchLcs(cls, seq("A")).get shouldBe logA_CDEF
      spell.matchLcs(cls, seq("AB")).get shouldBe logAB_DEF
      spell.matchLcs(cls, seq("ABC")).get shouldBe logA_CDEF
      spell.matchLcs(cls, seq("XB")).get shouldBe logAB_DEF

      spell.matchLcs(cls, seq("XBCYEZ")).get shouldBe log_BC_E_
      spell.matchLcs(cls, seq("XBCDEY")).get shouldBe log_BCDE_
      spell.matchLcs(cls, seq("BCDE")).get shouldBe log_BCDE_
      spell.matchLcs(cls, seq("BE")).get shouldBe logAB_DEF
      spell.matchLcs(cls, seq("BXYE")).get shouldBe logAB_DEF

      spell.matchLcs(cls, seq("XBCYZ")) shouldBe None
    }

    //TODO clear tests
    //TODO smaller template
    //TODO max template template
  }

  def clusters(logLineTypes: LogLineType*): Set[LogLineType] = logLineTypes.toSet

  def line(seqStr: String, id: UUID = UUID.randomUUID()): LogLineType = LogLineType(seq(seqStr), id)

  def seq(seqStr: String): TokenSeq = seqStr.map(_.toString)

  def list(seqStr: String): List[String] = seq(seqStr).toList

}