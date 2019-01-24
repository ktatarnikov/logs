package com.logprocessing.alg.spell

import java.util.UUID

import PrefixTree.PrefixTreeNode
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import com.logprocessing.log._

class PrefixTreeSpec extends WordSpec {

  val id1 = UUID.randomUUID()
  val id2 = UUID.randomUUID()
  val id3 = UUID.randomUUID()

  "prefix tree" should {
    "handle deleted from empty tree" in {
      val root = PrefixTreeNode()
      root.remove(LogLineType(seq("ABCD"))) shouldBe root
    }

    "handle matchedSeq against empty tree" in {
      PrefixTreeNode().matchSeq(seq("ABCD")) shouldBe None
    }

    "handle adding and matching of one element sequence to the tree" in {
      val tree = PrefixTreeNode().add(line("A", id1))
      tree.matchSeq(seq("A")) shouldBe Some(line("A", id1))
      tree.remove(line("A", id1)) shouldBe PrefixTreeNode()
    }

    "handle adding and matching of different two element sequences to the tree" in {

      val one = PrefixTreeNode().add(line("AB", id1))
      val two = one.add(line("CD", id2))
      val tree = two.add(line("EF", id3))

      tree.matchSeq(seq("AB")) shouldBe Some(line("AB", id1))
      tree.matchSeq(seq("CD")) shouldBe Some(line("CD", id2))
      tree.matchSeq(seq("EF")) shouldBe Some(line("EF", id3))

      tree.remove(line("EF", id3)) shouldBe two

      tree.remove(line("EF", id3))
        .remove(line("CD", id2)) shouldBe one

      tree.remove(line("EF", id3))
        .remove(line("CD", id2))
        .remove(line("AB", id1)) shouldBe PrefixTreeNode()
    }

    "handle adding and matching of several two element sequence to the tree" in {

      val one = PrefixTreeNode().add(line("AB", id1))
      val two = one.add(line("AC", id2))
      val tree = two.add(line("AD", id3))

      tree.matchSeq(seq("AB")) shouldBe Some(line("AB", id1))
      tree.matchSeq(seq("AC")) shouldBe Some(line("AC", id2))
      tree.matchSeq(seq("AD")) shouldBe Some(line("AD", id3))

      tree.remove(line("AD", id3)) shouldBe two

      tree.remove(line("AD", id3))
        .remove(line("AC", id2)) shouldBe one

      tree.remove(line("AD", id3))
        .remove(line("AC", id2))
        .remove(line("AB", id1)) shouldBe PrefixTreeNode()

    }

    "handle adding and matching of three element sequence to the tree" in {

      val one = PrefixTreeNode().add(line("ABD", id1))
      val two = one.add(line("ABC", id2))
      val tree = two.add(line("ABE", id3))

      tree.matchSeq(seq("ABD")) shouldBe Some(line("ABD", id1))
      tree.matchSeq(seq("ABC")) shouldBe Some(line("ABC", id2))
      tree.matchSeq(seq("ABE")) shouldBe Some(line("ABE", id3))

      tree.remove(line("ABE", id3)) shouldBe two

      tree.remove(line("ABE", id3))
        .remove(line("ABC", id2)) shouldBe one

      tree.remove(line("ABE", id3))
        .remove(line("ABC", id2))
        .remove(line("ABD", id1)) shouldBe PrefixTreeNode()

    }

    "handle templates with wildcards" in {

      val one = PrefixTreeNode().add(line("A*D", id1))
      val two = one.add(line("A*C", id2))
      val tree = two.add(line("A*E", id3))

      tree.matchSeq(seq("ABD")) shouldBe Some(line("A*D", id1))
      tree.matchSeq(seq("ABC")) shouldBe Some(line("A*C", id2))
      tree.matchSeq(seq("ABE")) shouldBe Some(line("A*E", id3))

      tree.remove(line("A*E", id3)) shouldBe two

      tree.remove(line("A*E", id3))
        .remove(line("A*C", id2)) shouldBe one

      tree.remove(line("A*E", id3))
        .remove(line("A*C", id2))
        .remove(line("A*D", id1)) shouldBe PrefixTreeNode()

    }

    "handle approximate queries" in {
      val one = PrefixTreeNode().add(line("A*C*", id1))
      val two = one.add(line("B**A", id2))
      val tree = two.add(line("C***B", id3))

      tree.matchSeq(seq("ABCD"), tau = 0.5) shouldBe Some(line("A*C*", id1))
      tree.matchSeq(seq("BCDA"), tau = 0.5) shouldBe Some(line("B**A", id2))
      tree.matchSeq(seq("CXYZB"), tau = 0.5) shouldBe None
      tree.matchSeq(seq("CXYB"), tau = 0.5) shouldBe Some(line("C***B", id3))

      tree.remove(line("C***B", id3)) shouldBe two

      tree.remove(line("C***B", id3))
        .remove(line("B**A", id2)) shouldBe one

      tree.remove(line("C***B", id3))
        .remove(line("B**A", id2))
        .remove(line("A*C*", id1)) shouldBe PrefixTreeNode()

    }

  }

  def line(seqStr: String, id: UUID): LogLineType = LogLineType(seq(seqStr), id)

  def list(seqStr: String): List[String] = seq(seqStr).toList

  def seq(seqStr: String): TokenSeq = seqStr.map(_.toString)
}

