package com.logprocessing.alg.spell

import java.util.UUID
import com.logprocessing.log._
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import com.logprocessing.alg.TestSupport._

class MatchLcsSpec extends WordSpec {

  val spell = Spell(null, tau = 0.5)
  val ids: Seq[EventType] = (0 to 5).map(_ => UUID.randomUUID())

  "matchLcs" should {
    "handle basic cases correctly" in {
      val logA = line("A")
      val logAB = line("AB")
      val logABC = line("ABC")
      val cls = clusters(logA, logAB, logABC)

      spell.matchLcs(cls, seq("N")) shouldBe None
      spell.matchLcs(cls, IndexedSeq.empty).get shouldBe logA
      spell.matchLcs(cls, seq("XYZ")) shouldBe None
      spell.matchLcs(cls, seq("A")).get shouldBe logA
      spell.matchLcs(cls, seq("AB")).get shouldBe logAB
      spell.matchLcs(cls, seq("ABC")).get shouldBe logABC
    }

    "find the maximally matching cluster" in {
      val log1 = line("A**DEFGH")
      val log2 = line("AB*DEFGH")
      val log3 = line("*BC*E*GH")
      val log4 = line("*BCDE*GH")
      val log5 = line("*B**E*GH")
      val cls = clusters(log1, log2, log3, log4, log5)

      spell.matchLcs(cls, seq("ABCDEFGH")).get shouldBe log2
    }

    "not match the clusters if the number of common tokens is less than half" in {
      val log1 = line("ABCDEFGH")
      val cls = clusters(log1)

      spell.matchLcs(cls, seq("ABCVWXYZ")) shouldBe None
      spell.matchLcs(cls, seq("ABCDWXYZ")).get shouldBe log1
    }

    "longer lcs is preffered" in {
      val log1 = line("ABCDEFGHIJ**M")
      val log2 = line("ABCDEFGHIJK*M")
      val cls = clusters(log1, log2)

      spell.matchLcs(cls, seq("ABCDEFGHIJKLM")).get shouldBe log2
    }

    "smaller template is preferred if the lcs is of the same size" in {
      val log1 = line("ABCDEFGHIJ**M")
      val log2 = line("ABCDEFGHIJ*M")
      val cls = clusters(log1, log2)

      spell.matchLcs(cls, seq("ABCDEFGHIJKLM")).get shouldBe log2
    }

    "the matching clusters should be at least half of the sequence" in {
      val log1 = line("ABCDEFGHIJ*M")
      val log2 = line("ABCDEFGHIJK*M")
      val cls = clusters(log1, log2)

      spell.matchLcs(cls, seq("ABCDEFGHIJKLM")).get shouldBe log2
      spell.matchLcs(cls, seq("ABCDEFGHIJKLM")).get shouldBe log1
    }

  }

}