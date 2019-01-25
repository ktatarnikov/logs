package com.logprocessing.alg.drain

import com.logprocessing.alg.drain.PrefixTree.{fastMatch, hasNumbers, mapOfTrees}
import com.logprocessing.log.{LogLineType, TokenSeq}

object PrefixTree {

  def sequenceDistance(template: TokenSeq, sequence: TokenSeq): (Double, Int) = {

    if (template.size != sequence.size) {
      throw new RuntimeException("Sequence sizes are not aligned.")
    }

    case class Result(numOfWildcards: Int = 0, numOfTokens: Int = 0)

    val result =
      template.zip(sequence).foldLeft(Result()) { case (acc, (tok1, tok2)) =>
        if (tok1 == "<*>") {
          acc.copy(numOfWildcards = acc.numOfWildcards + 1)
        } else if (tok1 == tok2) {
          acc.copy(numOfTokens = acc.numOfTokens + 1)
        } else {
          acc
        }
      }
    val similarity =
      if (template.isEmpty)
        0.0
      else if (template == sequence)
        1.0
      else
        result.numOfTokens.toDouble / template.size

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

  def hasNumbers(token: String): Boolean = "[0-9]+".r.findFirstMatchIn(token).exists(_ => true)

  def mapOfTrees(sequenceMax: Int, maxDepth: Int) : Map[Int, PrefixTreeNode] =
    (1 to sequenceMax).map(i => i -> PrefixTreeNode(maxDepth)).toMap
}

case class SequenceTreeRoot(maxDepth: Int,
                            seqTrees: Map[Int, PrefixTreeNode],
                            maxChild : Int = 100) {

  def clusters: Set[LogLineType] = seqTrees.values.flatMap(_.treeClusters).toSet

  def add(newLogLine: LogLineType): SequenceTreeRoot = {
    val templateLength = newLogLine.template.size

    seqTrees.get(templateLength) match {
      case Some(tree) =>
        this.copy(
          seqTrees = seqTrees.updated(templateLength, tree.add(newLogLine))
        )
      case None =>
        val newNode =
          PrefixTreeNode(maxDepth, token = "root", maxChild = maxChild).add(newLogLine)
        this.copy(
          seqTrees = seqTrees.updated(templateLength, newNode)
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

  def search(tokenSeq: TokenSeq, similarityThreshold: Double = 0.4): Option[LogLineType] = {
    seqTrees
      .get(tokenSeq.size)
      .flatMap(tree => {
        val clusters = tree.search(tokenSeq).getOrElse(Set.empty)
        fastMatch(clusters, tokenSeq, similarityThreshold)
      })
  }

}

case class PrefixTreeNode(maxDepth: Int,
                          children: Map[String, PrefixTreeNode] = Map(),
                          clusters: Set[LogLineType] = Set.empty,
                          token: String = "",
                          depth: Int = 1,
                          maxChild: Int = 100) {

  def treeClusters: Set[LogLineType] = clusters ++ children.values.flatMap{ child => child.treeClusters ++ child.clusters }

  def isEmpty : Boolean = children.size + clusters.size == 0

  def add(newLogLine: LogLineType): PrefixTreeNode = add(newLogLine, newLogLine.template.toList, 1)

  def add(newLogLine: LogLineType, tokenSeq: List[String], currentDepth: Int): PrefixTreeNode = {
    tokenSeq match {
      case Nil =>
        this.copy(
          clusters = this.clusters + newLogLine
        )
      case head :: tail =>
        if (currentDepth >= this.maxDepth || currentDepth > newLogLine.template.size) {
          this.copy(
            clusters = this.clusters + newLogLine
          )
        } else {
          (children.get(head), children.get("<*>")) match {
            case (None, Some(wildcard)) if hasNumbers(head) =>
              this.copy(
                children = children.updated("<*>", wildcard.add(newLogLine, tail, currentDepth + 1))
              )

            case (None, None) if hasNumbers(head) =>
              addNewNode("<*>", tail, newLogLine, currentDepth)

            case (None, Some(wildcard)) if !hasNumbers(head) =>
              if (this.children.size < maxChild) {
                addNewNode(head, tail, newLogLine, currentDepth)
              } else {
                this.copy(
                  children = children.updated("<*>", wildcard.add(newLogLine, tail, currentDepth + 1))
                )
              }

            case (None, None) if !hasNumbers(head) =>
              if (this.children.size + 1 < maxChild) {
                addNewNode(head, tail, newLogLine, currentDepth)
              } else if(this.children.size + 1 == maxChild) {
                addNewNode("<*>", tail, newLogLine, currentDepth)
              } else {
                ???
              }

            case (Some(child), _) =>
              this.copy(
                children = children.updated(head, child.add(newLogLine, tail, currentDepth + 1))
              )
          }
        }
    }
  }

  def addNewNode(head: String, tail: List[String], newLogLine: LogLineType, currentDepth: Int): PrefixTreeNode = {
    val newNode =
      PrefixTreeNode(maxDepth, token = head, maxChild = maxChild, depth = currentDepth + 1)
        .add(newLogLine, tail, currentDepth + 1)
    this.copy(
      children = children.updated(head, newNode)
    )
  }

  def remove(newLogLine: LogLineType): PrefixTreeNode = remove(newLogLine, newLogLine.template.toList, 1)

  def remove(newLogLine: LogLineType, tokenSeq: List[String], currentDepth: Int): PrefixTreeNode = {
    tokenSeq match {
      case Nil =>
        this.copy(
          clusters = this.clusters - newLogLine
        )
      case head :: tail =>
        if (currentDepth >= this.maxDepth || currentDepth > newLogLine.template.size) {
          this.copy(
            clusters = this.clusters - newLogLine
          )
        } else {
          (children.get(head), children.get("<*>")) match {
            case (Some(child), Some(wildcard)) =>
              remove(head, child, newLogLine, tail, currentDepth)
              .remove("<*>", wildcard, newLogLine, tail, currentDepth)

            case (Some(child), None) =>
              remove(head, child, newLogLine, tail, currentDepth)

            case (None, Some(wildcard)) =>
              remove("<*>", wildcard, newLogLine, tail, currentDepth)

            case (_, _) => this
          }
        }
    }
  }

  def remove(token: String, child: PrefixTreeNode, newLogLine: LogLineType, tail: List[String], currentDepth: Int): PrefixTreeNode = {
    val updatedWildCard = child.remove(newLogLine, tail, currentDepth + 1)
    if (!updatedWildCard.isEmpty) {
      this.copy(
        children = children.updated(token, updatedWildCard)
      )
    } else {
      this.copy(
        children = children - token
      )
    }
  }

  def search(tokenSeq: TokenSeq): Option[Set[LogLineType]] = this.search(tokenSeq.toList, 1, tokenSeq.size, None)

  def search(tokenSeq: List[String], currentDepth: Int, seqLen: Int, currentMatch: Option[Set[LogLineType]]): Option[Set[LogLineType]] = {
    if (currentDepth >= this.maxDepth || currentDepth > seqLen || tokenSeq == Nil) {
      Some(this.clusters).orElse(currentMatch)
    } else {
      tokenSeq match {
        case head :: tail =>
          children
            .get(tokenSeq.head)
            .orElse(children.get("<*>"))
            .fold(Option.empty[Set[LogLineType]]) { child =>
              child.search(tail, currentDepth + 1, seqLen, Some(child.clusters))
            }
      }
    }
  }

}