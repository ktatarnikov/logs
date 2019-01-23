package com.stackstate.alg.spell

import com.stackstate.alg.LogSplitter._
import com.stackstate.alg.spell.PrefixTree.PrefixTreeNode
import com.stackstate.alg.spell.Spell._
import com.stackstate.alg.log._
import scala.annotation.tailrec

object Spell {

  def longestCommonSubseq(seq1: TokenSeq, seq2: TokenSeq): TokenSeq = {
    val dp = Array.ofDim[Int](seq1.length + 1, seq2.length + 1)
    for (i <- 1 to seq1.length) {
      for (j <- 1 to seq2.length) {
        if (seq1(i - 1) == seq2(j - 1)) {
          dp(i)(j) = 1 + dp(i - 1)(j - 1)
        } else {
          dp(i)(j) = Math.max(dp(i - 1)(j), dp(i)(j - 1))
        }
      }
    }

    @tailrec
    def recourse(current: List[String], len1: Int, len2: Int): TokenSeq = {
      if (len1 == 0 || len2 == 0)
        current.toIndexedSeq
      else {
        if (dp(len1)(len2) == dp(len1 - 1)(len2)) {
          recourse(current, len1 - 1, len2)
        } else if (dp(len1)(len2) == dp(len1)(len2 - 1)) {
          recourse(current, len1, len2 - 1)
        } else {
          recourse(seq1(len1 - 1) :: current, len1 - 1, len2 - 1)
        }
      }
    }

    recourse(List.empty, seq1.length, seq2.length)
  }

  def getTemplate(lcs: TokenSeq, seq: TokenSeq): TokenSeq = {
    if (lcs.isEmpty)
      IndexedSeq.empty
    else {

      def recourse(seq: List[String], lcs: List[String]): List[String] = {
        (seq, lcs) match {
          case (Nil, _) => Nil
          case (h, Nil) => "*" :: Nil
          case (h :: t, lcsSeq) if h == lcsSeq.head => h :: recourse(t, lcsSeq.tail)
          case (h :: t, lcsSeq) if h != lcsSeq.head => "*" :: recourse(t, lcsSeq)
        }
      }

      recourse(seq.toList, lcs.toList).toIndexedSeq
    }
  }
}

case class Spell(splitter: LogLineSplitter,
                 tau: Double = 0.5f,
                 clusters: Set[LogLineType] = Set.empty,
                 prefixTree: PrefixTreeNode = PrefixTreeNode()) {

  def parse(text: String): (Spell, EventType, LogLine) = {
    val logLine = splitter.split(text)
    val tokenSeq = logLine.contents
    val constantSeq = tokenSeq.filter(_ != "*")

    matchPrefixTree(constantSeq)
      .orElse(matchTypeDirect(this.clusters, constantSeq))
      .orElse(matchLcs(this.clusters, constantSeq))
      .fold {
        val newType = LogLineType(tokenSeq)

        (
          this.copy(
            clusters = clusters + newType,
            prefixTree = prefixTree.add(newType)
          ),
          newType.eventType,
          logLine
        )
      } { logLineType =>
        val newTemplate = getTemplate(longestCommonSubseq(tokenSeq, logLineType.template), logLineType.template)
        if (!newTemplate.equals(logLineType.template)) {
          val treeWithoutType = prefixTree.remove(logLineType)
          val clustersWithoutType = clusters - logLineType
          val updatedLogType = logLineType.copy(newTemplate)
          (
            this.copy(
              clusters = clustersWithoutType + updatedLogType,
              prefixTree = treeWithoutType.add(updatedLogType)
            ),
            updatedLogType.eventType,
            logLine
          )
        } else
          (this, logLineType.eventType, logLine)
      }
  }

  def matchTypeDirect(clusters: Set[LogLineType], constantSeq: TokenSeq): Option[LogLineType] = {
    val seq = constantSeq.toSet

    def matchCluster(logLineType: LogLineType): Boolean = {
      logLineType.template.size >= this.tau * seq.size &&
        logLineType.template.forall(token => seq.contains(token) || "*" == token)
    }

    clusters.find(matchCluster)
  }

  protected def matchPrefixTree(constantSeq: TokenSeq): Option[LogLineType] = {
    prefixTree.matchSeq(constantSeq, this.tau)
  }

  def matchLcs(clusters: Set[LogLineType], constantSeq: TokenSeq): Option[LogLineType] = {
    val seq = constantSeq.toSet

    case class MaxCluster(logLineType: Option[LogLineType] = None, length: Int = -1)

    val maxCluster =
      clusters.foldLeft(MaxCluster()) { (max, logLineType) =>
        val template = logLineType.template.toSet
        if (template.intersect(seq).size < 0.5 * seq.size) {
          max
        } else {
          val lcsSeq = longestCommonSubseq(constantSeq, logLineType.template)
          val lcsMoreThanCurrentMax = lcsSeq.size > max.length
          val smallerTemplate = lcsSeq.size == max.length && logLineType.template.size < max.logLineType.map(_.template.size).getOrElse(0)
          if (lcsMoreThanCurrentMax || smallerTemplate) {
            max.copy(
              logLineType = Some(logLineType),
              length = lcsSeq.size
            )
          } else
            max
        }
      }
    maxCluster.logLineType.flatMap { logLineType =>
      if (maxCluster.length >= this.tau * constantSeq.size)
        Some(logLineType)
      else
        None
    }
  }

}
