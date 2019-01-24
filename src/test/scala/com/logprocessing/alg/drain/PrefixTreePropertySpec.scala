package com.logprocessing.alg.drain

import com.logprocessing.alg.TestSupport._
import org.scalatest.AppendedClues._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class PrefixTreePropertySpec extends WordSpec {

  "SequenceTreeRoot" should {
    "be able to add and remove 20K components and preserve tree invariant" in {
      val logLineTypes = generate(20000, 1, 10)
      println(s"Processing ${logLineTypes.size} logLineTypes.")
      var trees = List.empty[SequenceTreeRoot]
      var tree = SequenceTreeRoot(maxChild = 50)

      for (logLineType <- logLineTypes) {
//        tree.search(logLineType.template) shouldBe None withClue s"Searching for ${logLineType.template}"
        tree = tree.add(logLineType)
        trees ::= tree
        tree.search(logLineType.template) shouldBe Some(logLineType) withClue s"Searching for ${logLineType.template}"
      }

      tree.clusters shouldBe logLineTypes.toSet

      for (logLineType <- logLineTypes.reverse) {
        val head :: tail = trees
        tree shouldBe head
        trees = tail
        tree.search(logLineType.template) shouldBe Some(logLineType) withClue s"Searching for ${logLineType.template}"
        tree = tree.remove(logLineType)
//        tree.search(logLineType.template) shouldBe None withClue s"Searching for ${logLineType.template}"
      }
      tree shouldBe SequenceTreeRoot(maxChild = 50)
    }

  }
}
