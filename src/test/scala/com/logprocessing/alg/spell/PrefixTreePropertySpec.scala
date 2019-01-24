package com.logprocessing.alg.spell

import com.logprocessing.alg.TestSupport._
import com.logprocessing.alg.drain.SequenceTreeRoot
import com.logprocessing.alg.spell.PrefixTree.PrefixTreeNode
import org.scalatest.AppendedClues._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class PrefixTreePropertySpec extends WordSpec {

  "PrefixTreeNode" should {
    "be able to add and remove 10K components" in {
      val logLineTypes = generate(100, 1, 10)
      var trees = IndexedSeq.empty[PrefixTreeNode]
      var tree = PrefixTreeNode()

      for (logLineType <- logLineTypes) {
        tree.matchSeq(logLineType.template) shouldBe None withClue s"Searching for ${logLineType.template}"
        tree = tree.add(logLineType)
        trees :+= tree
        tree.matchSeq(logLineType.template) shouldBe Some(logLineType)
      }

//      tree.clusters shouldBe logLineTypes.toSet

      for (logLineType <- logLineTypes.reverse) {
        tree.matchSeq(logLineType.template) shouldBe Some(logLineType)
        tree = tree.remove(logLineType)
        tree.matchSeq(logLineType.template) shouldBe None
      }
      tree shouldBe SequenceTreeRoot(maxChild = 10)
    }
  }
}
