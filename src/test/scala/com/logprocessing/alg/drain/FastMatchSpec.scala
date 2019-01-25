package com.logprocessing.alg.drain

import java.util.UUID

import com.logprocessing.alg.TestSupport._
import com.logprocessing.alg.drain.PrefixTree.fastMatch
import com.logprocessing.log.{LogLineType, TokenSeq}
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class FastMatchSpec extends WordSpec {

  def line(seqStr: String, id: UUID = UUID.randomUUID()): LogLineType =
    LogLineType(seq(seqStr), id)

  def seq(seqStr: String): TokenSeq = seqStr.map {
    case '*' => "<*>"
    case other => other.toString
  }

  val logABCX = line("ABCX")
  val logABDY = line("ABDY")
  val logABEZ = line("ABEZ")
  val logA__M = line("A**M")
  val cls = clusters(logABCX, logABDY, logABEZ, logA__M)

  "FastMatch" should {
    "handle basic cases correctly" in {
      fastMatch(cls, seq("ZDKE"), 0.4) shouldBe None
      fastMatch(cls, seq("ABCX"), 0.4) shouldBe Some(logABCX)
      fastMatch(cls, seq("AXXM"), 0.4) shouldBe Some(logA__M)
    }

    "match the same cluster" in {
      val log1 = line("A****")
      val cls = clusters(log1)
      fastMatch(cls, seq("A****"), 0.4) shouldBe Some(log1)
      fastMatch(cls, seq("A****"), 0.0) shouldBe Some(log1)
      fastMatch(cls, seq("A****"), 1.0) shouldBe Some(log1)
    }

    "match the most specific cluster" in {
      val log1 = line("A****")
      val log2 = line("AB***")
      val log3 = line("ABC**")
      val cls = clusters(log1, log2, log3)
      fastMatch(cls, seq("ABCDE"), 0.4) shouldBe Some(log3)
    }

    "match the most specific from all cluster" in {
      val log1 = line("A***E***I")
      val log2 = line("A*C******")
      val log3 = line("*B*D*****")
      val cls = clusters(log1, log2, log3)
      fastMatch(cls, seq("ABCDEFGHI"), 0.0) shouldBe Some(log1)
      // does not match log1, because I and <*> a considered different
      fastMatch(cls, seq("ABCDEFGH*"), 0.0) shouldBe Some(log2)

    }

  }

}
