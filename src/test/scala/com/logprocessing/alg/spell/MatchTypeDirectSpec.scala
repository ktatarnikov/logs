package com.logprocessing.alg.spell

import com.logprocessing.alg.TestSupport._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class MatchTypeDirectSpec extends WordSpec {

  val logA = line("A")
  val logAB = line("AB")
  val logABC = line("ABC")
  val cls = clusters(logA, logAB, logABC)

  val spell = Spell(null, tau = 0.5)
  "matchTypeDirect" should {
    "not match empty clusters" in {
      spell.matchTypeDirect(Set.empty, seq("A")) shouldBe None
    }

    "match one cluster map" in {
      spell.matchTypeDirect(clusters(logA), seq("A")).get shouldBe logA
      spell.matchTypeDirect(clusters(logAB), seq("AB")).get shouldBe logAB
      spell.matchTypeDirect(clusters(logABC), seq("ABC")).get shouldBe logABC
    }

    "match one cluster in map of several" in {
      spell.matchTypeDirect(cls, seq("A")).get shouldBe logA
      spell.matchTypeDirect(cls, seq("AB")).get shouldBe logA
      spell.matchTypeDirect(cls, seq("ABC")).get shouldBe logAB
    }

    "match with different order" in {
      spell.matchTypeDirect(cls, seq("BCA")).get shouldBe logAB
    }
  }

}
