package com.stackstate.alg.drain

import com.stackstate.alg.drain.PrefixTree.fastMatch
import com.stackstate.alg.log.{LogLineType, TokenSeq}

object PrefixTree {

  def sequenceDistance(template: TokenSeq, sequence: TokenSeq): (Double, Int) = {

    if (template.size != sequence.size) {
      throw new RuntimeException("Sequence sizes are not aligned.")
    }

    case class Result(numOfWildcards: Int = 0, numOfTokens: Int = 0)

    val result =
      template.zip(sequence).foldLeft(Result()) { case (acc, (tok1, tok2)) =>
        acc.copy(
          numOfWildcards = acc.numOfWildcards + (if (tok1 == "<*>") 1 else 0),
          numOfTokens = acc.numOfTokens + (if (tok1 != "<*>" && tok1 == tok2) 1 else 0)
        )
      }
    val similarity = if (template.isEmpty) 0 else result.numOfTokens.toDouble / template.size
    (similarity, result.numOfWildcards)
  }

  def fastMatch(logLineTypes: Set[LogLineType], tokenSeq: TokenSeq, similarityThreshold: Double): Option[LogLineType] = {

    case class MatchResult(maxSimilarity: Double = -1,
                           maxNumberOfWildCards: Int = -1,
                           bestMatch: Option[LogLineType] = None)

    val result =
      logLineTypes.foldLeft(MatchResult()) { case (acc, current: LogLineType) =>
        val (similarity, numberOfWildcards) = sequenceDistance(current.template, tokenSeq)
        if (similarity > acc.maxSimilarity || (similarity == acc.maxSimilarity && numberOfWildcards > acc.maxNumberOfWildCards)) {
          acc.copy(
            similarity,
            numberOfWildcards,
            Some(current)
          )
        } else
          acc
      }

    if (result.maxSimilarity >= similarityThreshold) {
      result.bestMatch
    } else
      None
  }

}

case class SequenceTreeRoot(seqTrees: Map[Int, PrefixTreeNode] = Map()) {

  def add(newLogLine: LogLineType): SequenceTreeRoot = {
    val templateLength = newLogLine.template.size

    seqTrees.get(templateLength) match {
      case Some(tree) =>
        this.copy(
          seqTrees = seqTrees.updated(templateLength, tree.add(newLogLine))
        )
      case None =>
        this.copy(
          seqTrees = seqTrees.updated(templateLength, PrefixTreeNode().add(newLogLine))
        )
    }
  }

  def remove(newLogLine: LogLineType): SequenceTreeRoot = {
    val templateLength = newLogLine.template.size

    seqTrees.get(templateLength) match {
      case Some(tree) =>
        this.copy(
          seqTrees = seqTrees.updated(templateLength, tree.remove(newLogLine))
        )
      case None =>
        this
    }
  }

  def search(tokenSeq: TokenSeq, similarityThreshold: Double): Option[LogLineType] = {
    seqTrees
      .get(tokenSeq.size)
      .flatMap(tree => {
        val treeSearchResult = tree.search(tokenSeq.toList, 1, tokenSeq.size, None)
        val clusters = treeSearchResult.getOrElse(Set.empty)
        fastMatch(clusters, tokenSeq, similarityThreshold)
      })
  }

}

case class PrefixTreeNode(children: Map[String, PrefixTreeNode] = Map(),
                          clusters: Set[LogLineType] = Set.empty,
                          seqTrees: Map[Int, PrefixTreeNode] = Map(),
                          token: String = "",
                          depth: Int = 0) {

  def add(newLogLine: LogLineType): PrefixTreeNode = {

    
    this
  }

  def remove(newLogLine: LogLineType): PrefixTreeNode = {
    this
  }

  def search(tokenSeq: List[String], currentDepth: Int, seqLen: Int, currentMatch: Option[Set[LogLineType]]): Option[Set[LogLineType]] = {
    if (currentDepth >= this.depth || currentDepth > seqLen) {
      currentMatch
    } else {
      tokenSeq match {
        case head :: tail =>
          children
            .get(tokenSeq.head)
            .orElse(children.get("<*>"))
            .fold(Option.empty[Set[LogLineType]]) { child =>
              child.search(tail, currentDepth + 1, seqLen, Some(child.clusters))
            }

        case Nil => currentMatch
      }
    }
  }

}