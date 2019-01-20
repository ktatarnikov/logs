package com.stackstate.alg.spell

import com.stackstate.alg.spell.Spell.{Cluster, Node, TokenSeq, Tree}

object Spell {

  type Token = String
  type TokenSeq = IndexedSeq[Token]
  type EventType = Long
  type Cluster = TokenSeq
  trait Tree
  case class Leaf(token: Token) extends Tree
  case class Node(trees: Map[Token, Tree] = Map()) extends Tree
}

class Spell() {
  val clusters: Cluster = IndexedSeq()
  val prefixTree: Tree = Node()

  def lcs(seq1: TokenSeq, seq2: TokenSeq): TokenSeq = {
    val dp = Array.ofDim[Int](seq1.length + 1, seq2.length + 1)
    for (i <- 1 to seq1.length) {
      for (j <- 1 to seq2.length) {
        if (seq1(i) == seq2(j)) {
          dp(i)(j) = 1 + dp(i - 1)(j - 1)
        } else {
          dp(i)(j) = Math.max(dp(i - 1)(j), dp(i)(j - 1))
        }
      }
    }
    var result : TokenSeq = Vector.empty
    var length1 = seq1.length
    var length2 = seq2.length

    while (length1 != 0 && length2 != 0) {
      if (dp(length1)(length2) == dp(length1 - 1)(length2)) {
        length1 -= 1
      } else if (dp(length1)(length2) == dp(length1)(length2 - 1)) {
        length2 -= 1
      } else {
        result +:= seq1(length1 - 1)
        length1 -= 1
        length2 -= 1
      }
    }
    result
  }
}
