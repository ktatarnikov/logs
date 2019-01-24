package com.logprocessing.alg.drain

import com.logprocessing.alg.drain.PrefixTree.sequenceDistance
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class SequenceDistanceSpec extends WordSpec {

  "SequenceDistance" should {
    "handle basic cases correctly" in {
      sequenceDistance(IndexedSeq.empty, IndexedSeq.empty) shouldBe (0, 0)
      sequenceDistance(IndexedSeq("A"), IndexedSeq("B")) shouldBe (0, 0)
      sequenceDistance(IndexedSeq("A"), IndexedSeq("A")) shouldBe (1, 0)

      sequenceDistance(IndexedSeq("<*>"), IndexedSeq("B")) shouldBe (0, 1)

      sequenceDistance(IndexedSeq("A", "<*>"), IndexedSeq("A", "B")) shouldBe (0.5, 1)
      sequenceDistance(IndexedSeq("<*>", "A"), IndexedSeq("B", "A")) shouldBe (0.5, 1)
      sequenceDistance(IndexedSeq("<*>", "<*>"), IndexedSeq("B", "A")) shouldBe (0.0, 2)
      sequenceDistance(IndexedSeq("A", "B"), IndexedSeq("A", "B")) shouldBe (1, 0)

      sequenceDistance(IndexedSeq("A", "B", "C"), IndexedSeq("A", "B", "C")) shouldBe (1, 0)
      sequenceDistance(IndexedSeq("<*>", "B", "C"), IndexedSeq("A", "B", "C")) shouldBe (2.0/3.0, 1)
      sequenceDistance(IndexedSeq("A", "<*>", "C"), IndexedSeq("A", "B", "C")) shouldBe (2.0/3.0, 1)
      sequenceDistance(IndexedSeq("A", "B", "<*>"), IndexedSeq("A", "B", "C")) shouldBe (2.0/3.0, 1)
      sequenceDistance(IndexedSeq("<*>", "B", "<*>"), IndexedSeq("A", "B", "C")) shouldBe (1.0/3.0, 2)
      sequenceDistance(IndexedSeq("<*>", "<*>", "<*>"), IndexedSeq("A", "B", "C")) shouldBe (0, 3)
    }
  }
}
