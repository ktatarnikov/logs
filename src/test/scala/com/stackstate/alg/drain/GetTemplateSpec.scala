package com.stackstate.alg.drain

import com.stackstate.alg.LogSplitter._
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import com.stackstate.alg.log._
import com.stackstate.alg.drain.Drain.getTemplate

class GetTemplateSpec extends WordSpec {

  "getTemplate" should {
    "handle basic cases correctly" in {
      getTemplate(IndexedSeq(), IndexedSeq()) shouldBe IndexedSeq()
      getTemplate(IndexedSeq("A"), IndexedSeq("A")) shouldBe IndexedSeq("A")
      getTemplate(IndexedSeq("A"), IndexedSeq("B")) shouldBe IndexedSeq("*")

      getTemplate(IndexedSeq("A", "B"), IndexedSeq("A", "B")) shouldBe IndexedSeq("A", "B")
      getTemplate(IndexedSeq("B", "C"), IndexedSeq("A", "C")) shouldBe IndexedSeq("*", "C")
      getTemplate(IndexedSeq("A", "B"), IndexedSeq("A", "C")) shouldBe IndexedSeq("A", "*")

      getTemplate(IndexedSeq("A", "B", "C"), IndexedSeq("X", "B", "Z")) shouldBe IndexedSeq("*", "B", "*")

    }
  }
}
