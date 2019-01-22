package com.stackstate.alg.spell

import com.stackstate.alg.spell.Spell._
import org.scalatest.Matchers._

import org.scalatest.WordSpec

class GetTemplateSpec extends WordSpec {
  "getTemplate" should {
    "handle basic cases correctly" in {
        getTemplate(IndexedSeq(), IndexedSeq()) shouldBe IndexedSeq()
        getTemplate(IndexedSeq("A"), IndexedSeq()) shouldBe IndexedSeq()
        getTemplate(IndexedSeq(), IndexedSeq("A")) shouldBe IndexedSeq()
        getTemplate(IndexedSeq("A"), IndexedSeq("A")) shouldBe IndexedSeq("A")
        getTemplate(IndexedSeq("A"), IndexedSeq("B")) shouldBe IndexedSeq("*")

        getTemplate(IndexedSeq(), IndexedSeq("C", "B")) shouldBe IndexedSeq()
        getTemplate(IndexedSeq("B"), IndexedSeq("C", "B")) shouldBe IndexedSeq("*", "B")
        getTemplate(IndexedSeq("C"), IndexedSeq("C", "B")) shouldBe IndexedSeq("C", "*")
        getTemplate(IndexedSeq("A", "B"), IndexedSeq("A", "B", "C")) shouldBe IndexedSeq("A", "B", "*")
        getTemplate(IndexedSeq("B", "C"), IndexedSeq("A", "B", "C")) shouldBe IndexedSeq("*", "B", "C")
        getTemplate(IndexedSeq("B", "C"), IndexedSeq("A", "B", "C")) shouldBe IndexedSeq("*", "B", "C")

        getTemplate(IndexedSeq("B", "D"), IndexedSeq("A", "B", "C", "D")) shouldBe IndexedSeq("*", "B", "*", "D")

    }
  }
}
