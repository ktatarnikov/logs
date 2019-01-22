package com.stackstate.alg.spell

import com.stackstate.alg.spell.Spell._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class LcsSpec extends WordSpec {
  "lcs" should {
    "handle basic cases correctly" in {
      lcs(IndexedSeq(), IndexedSeq()) shouldBe IndexedSeq()
      lcs(IndexedSeq(), IndexedSeq("A")) shouldBe IndexedSeq()
      lcs(IndexedSeq("A"), IndexedSeq()) shouldBe IndexedSeq()
      lcs(IndexedSeq("A"), IndexedSeq("B")) shouldBe IndexedSeq()
      lcs(IndexedSeq("A"), IndexedSeq("A")) shouldBe IndexedSeq("A")
      lcs(IndexedSeq("A", "B"), IndexedSeq("A", "B")) shouldBe IndexedSeq("A", "B")
      lcs(IndexedSeq("A","B","C"), IndexedSeq("A","B","C")) shouldBe IndexedSeq("A","B","C")

      lcs(IndexedSeq("A","B"), IndexedSeq("A","C")) shouldBe IndexedSeq("A")
      lcs(IndexedSeq("A","B"), IndexedSeq("C","B")) shouldBe IndexedSeq("B")
      lcs(IndexedSeq("A","B","C"), IndexedSeq("X","B","Y")) shouldBe IndexedSeq("B")
    }
    //TODO add property based tests
  }
}
