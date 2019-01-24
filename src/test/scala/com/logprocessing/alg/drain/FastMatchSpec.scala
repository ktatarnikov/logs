package com.logprocessing.alg.drain

import com.logprocessing.alg.TestSupport._
import com.logprocessing.alg.drain.PrefixTree.fastMatch
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class FastMatchSpec extends WordSpec {

  val logABC = line("ABC")
  val logABD = line("ABD")
  val logABE = line("ABE")
  val cls = clusters(logABC, logABD, logABE)

  "FastMatch" should {
    "handle basic cases correctly" in {
      fastMatch(cls, seq("ZDK"), 0.4) shouldBe None
      //TODO tests
    }
  }

}
