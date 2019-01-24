package com.logprocessing.alg.drain

import com.logprocessing.log.LogLineType
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import org.scalatest.AppendedClues._
import scala.util.Random
import com.logprocessing.alg.TestSupport._

class PrefixTreePropertySpec extends WordSpec {

  "SequenceTreeRoot" should {
    "be able to add and remove 10K components" in {
      val logLineTypes = generate(100, 1, 10)
      var trees = IndexedSeq.empty[SequenceTreeRoot]
      var tree = SequenceTreeRoot(maxChild = 10)

      for (logLineType <- logLineTypes) {
        tree.search(logLineType.template) shouldBe None withClue s"Searching for ${logLineType.template}"
        tree = tree.add(logLineType)
        trees :+= tree
        tree.search(logLineType.template) shouldBe Some(logLineType)
      }

      tree.clusters shouldBe logLineTypes.toSet

      for (logLineType <- logLineTypes.reverse) {
        tree.search(logLineType.template) shouldBe Some(logLineType)
        tree = tree.remove(logLineType)
        tree.search(logLineType.template) shouldBe None
      }
      tree shouldBe SequenceTreeRoot(maxChild = 10)
    }
  }
}
