package com.logprocessing.alg.spell

import com.logprocessing.alg.TestSupport._
import com.logprocessing.alg.spell.PrefixTree.PrefixTreeNode
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class PrefixTreePropertySpec extends WordSpec {

  "PrefixTreeNode" should {
    "be able to add and remove 20K components and preserve the tree invariant" in {
      val logLineTypes = generate(20000, 1, 10)
      println(s"Processing ${logLineTypes.size} logLineTypes.")
      var trees = List.empty[PrefixTreeNode]

      var tree = PrefixTreeNode()
      for (logLineType <- logLineTypes) {
        tree = tree.add(logLineType)
        trees ::= tree
      }

      tree.logNum shouldBe logLineTypes.size

      for (logLineType <- logLineTypes.reverse) {
        val head :: tail = trees
        tree shouldBe head
        trees = tail
        tree = tree.remove(logLineType)
      }
      tree shouldBe PrefixTreeNode()
    }
  }
}
