package com.stackstate.alg.drain

import java.util.UUID

import com.stackstate.alg.drain.PrefixTree.fastMatch
import com.stackstate.alg.log.{LogLineType, TokenSeq}
import org.scalatest.WordSpec
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

  def clusters(logLineTypes: LogLineType*): Set[LogLineType] = logLineTypes.toSet

  def line(seqStr: String, id: UUID = UUID.randomUUID()): LogLineType = LogLineType(seq(seqStr), id)

  def seq(seqStr: String): TokenSeq = seqStr.map(_.toString)

  def list(seqStr: String): List[String] = seq(seqStr).toList

}
