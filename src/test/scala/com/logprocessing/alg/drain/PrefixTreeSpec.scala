package com.logprocessing.alg.drain


import java.util.UUID

import com.logprocessing.alg.drain.PrefixTree.hasNumbers
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import com.logprocessing.log._
import com.logprocessing.alg.TestSupport._

class PrefixTreeSpec extends WordSpec {

  val id1 = UUID.randomUUID()
  val id2 = UUID.randomUUID()
  val id3 = UUID.randomUUID()

  "sequence tree root" should {
    "handle deleted from empty tree" in {
      SequenceTreeRoot().remove(LogLineType(seq("ABCD"))) shouldBe SequenceTreeRoot()
    }

    "handle matchedSeq against empty tree" in {
      SequenceTreeRoot().search(seq("ABCD")) shouldBe None
    }

    "handle adding and matching of one element sequence to the tree" in {
      val tree = SequenceTreeRoot().add(line("A", id1))
      tree.search(seq("A")) shouldBe Some(line("A", id1))
      val root = tree.remove(line("A", id1))
      root shouldBe SequenceTreeRoot()
    }

    "handle adding and matching of different two element sequences to the tree" in {

      val one = SequenceTreeRoot().add(line("AB", id1))
      val two = one.add(line("CD", id2))
      val tree = two.add(line("EF", id3))

      tree.search(seq("AB")) shouldBe Some(line("AB", id1))
      tree.search(seq("CD")) shouldBe Some(line("CD", id2))
      tree.search(seq("EF")) shouldBe Some(line("EF", id3))

      tree.remove(line("EF", id3)) shouldBe two

      tree.remove(line("EF", id3))
        .remove(line("CD", id2)) shouldBe one

      tree.remove(line("EF", id3))
        .remove(line("CD", id2))
        .remove(line("AB", id1)) shouldBe SequenceTreeRoot()
    }

    "handle adding and matching of several two element sequence to the tree" in {

      val one = SequenceTreeRoot().add(line("AB", id1))
      val two = one.add(line("AC", id2))
      val tree = two.add(line("AD", id3))

      tree.search(seq("AB")) shouldBe Some(line("AB", id1))
      tree.search(seq("AC")) shouldBe Some(line("AC", id2))
      tree.search(seq("AD")) shouldBe Some(line("AD", id3))

      tree.remove(line("AD", id3)) shouldBe two

      tree.remove(line("AD", id3))
        .remove(line("AC", id2)) shouldBe one

      tree.remove(line("AD", id3))
        .remove(line("AC", id2))
        .remove(line("AB", id1)) shouldBe SequenceTreeRoot()

    }

    "handle adding and matching of three element sequence to the tree" in {

      val one = SequenceTreeRoot().add(line("ABD", id1))
      val two = one.add(line("ABC", id2))
      val tree = two.add(line("ABE", id3))

      tree.search(seq("ABD")) shouldBe Some(line("ABD", id1))
      tree.search(seq("ABC")) shouldBe Some(line("ABC", id2))
      tree.search(seq("ABE")) shouldBe Some(line("ABE", id3))

      tree.remove(line("ABE", id3)) shouldBe two

      tree.remove(line("ABE", id3))
        .remove(line("ABC", id2)) shouldBe one

      tree.remove(line("ABE", id3))
        .remove(line("ABC", id2))
        .remove(line("ABD", id1)) shouldBe SequenceTreeRoot()

    }

  }

  "prefix tree" should {
    "handle deleted from empty tree" in {
      PrefixTreeNode().remove(LogLineType(seq("ABCD"))) shouldBe PrefixTreeNode()
    }

    "handle matchedSeq against empty tree" in {
      PrefixTreeNode().search(seq("ABCD")) shouldBe clusters()
    }

    "handle adding and matching of one element sequence to the tree" in {
      val tree = PrefixTreeNode().add(line("A", id1))
      tree.search(seq("A")) shouldBe clusters(line("A", id1))
      tree.remove(line("A", id1)) shouldBe PrefixTreeNode()
    }

    "handle adding and matching of different two element sequences to the tree" in {

      val one = PrefixTreeNode().add(line("AB", id1))
      val two = one.add(line("CD", id2))
      val tree = two.add(line("EF", id3))

      tree.search(seq("AB")) shouldBe clusters(line("AB", id1), line("CD", id2), line("EF", id3))
      tree.search(seq("CD")) shouldBe clusters(line("AB", id1), line("CD", id2), line("EF", id3))
      tree.search(seq("EF")) shouldBe clusters(line("AB", id1), line("CD", id2), line("EF", id3))

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

      tree.search(seq("AB")) shouldBe clusters(line("AB", id1), line("AC", id2), line("AD", id3))
      tree.search(seq("AC")) shouldBe clusters(line("AB", id1), line("AC", id2), line("AD", id3))
      tree.search(seq("AD")) shouldBe clusters(line("AB", id1), line("AC", id2), line("AD", id3))

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

      tree.search(seq("ABD")) shouldBe clusters(line("ABD", id1), line("ABC", id2), line("ABE", id3))
      tree.search(seq("ABC")) shouldBe clusters(line("ABD", id1), line("ABC", id2), line("ABE", id3))
      tree.search(seq("ABE")) shouldBe clusters(line("ABD", id1), line("ABC", id2), line("ABE", id3))

      tree.remove(line("ABE", id3)) shouldBe two

      tree.remove(line("ABE", id3))
        .remove(line("ABC", id2)) shouldBe one

      tree.remove(line("ABE", id3))
        .remove(line("ABC", id2))
        .remove(line("ABD", id1)) shouldBe PrefixTreeNode()

    }

  }

  "hasNumbers" should {
    "return false if numbers is empty string" in {
      hasNumbers("") shouldBe false
    }

    "return false if string contains no numbers" in {
      hasNumbers("no numbers! !@#$%^&*") shouldBe false
    }

    "return true if string contains numbers" in {
      hasNumbers("qw1er") shouldBe true
    }

  }

}

