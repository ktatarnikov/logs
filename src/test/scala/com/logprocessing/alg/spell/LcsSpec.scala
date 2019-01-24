package com.logprocessing.alg.spell

import Spell._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class LcsSpec extends WordSpec {
  "lcs" should {
    "handle basic cases correctly" in {
      longestCommonSubseq(IndexedSeq(), IndexedSeq()) shouldBe IndexedSeq()
      longestCommonSubseq(IndexedSeq(), IndexedSeq("A")) shouldBe IndexedSeq()
      longestCommonSubseq(IndexedSeq("A"), IndexedSeq()) shouldBe IndexedSeq()
      longestCommonSubseq(IndexedSeq("A"), IndexedSeq("B")) shouldBe IndexedSeq()
      longestCommonSubseq(IndexedSeq("A"), IndexedSeq("A")) shouldBe IndexedSeq("A")
      longestCommonSubseq(IndexedSeq("A", "B"), IndexedSeq("A", "B")) shouldBe IndexedSeq("A", "B")
      longestCommonSubseq(IndexedSeq("A", "B", "C"), IndexedSeq("A", "B", "C")) shouldBe IndexedSeq("A", "B", "C")

      longestCommonSubseq(IndexedSeq("A", "B"), IndexedSeq("A", "C")) shouldBe IndexedSeq("A")
      longestCommonSubseq(IndexedSeq("A", "B"), IndexedSeq("C", "B")) shouldBe IndexedSeq("B")
      longestCommonSubseq(IndexedSeq("A", "B", "C"), IndexedSeq("X", "B", "Y")) shouldBe IndexedSeq("B")
    }
    //TODO add property based tests
  }
}
