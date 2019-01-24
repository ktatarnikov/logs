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

  def hasNumbers(token: String): Boolean = "[0-9]+".r.findFirstMatchIn(token).exists(_ => true)

  def mapOfTrees(sequenceMax: Int) : Map[Int, PrefixTreeNode] =
    (1 to sequenceMax).map(i => i -> PrefixTreeNode()).toMap
}

case class SequenceTreeRoot(seqTrees: Map[Int, PrefixTreeNode] = mapOfTrees(10), maxChild : Int = 100) {

  def clusters: Set[LogLineType] = seqTrees.values.flatMap(_.clusters).toSet

  def add(newLogLine: LogLineType): SequenceTreeRoot = {
    val templateLength = newLogLine.template.size

    seqTrees.get(templateLength) match {
      case Some(tree) =>
        this.copy(
          seqTrees = seqTrees.updated(templateLength, tree.add(newLogLine, newLogLine.template.toList, 1))
        )
      case None =>
        val newNode = PrefixTreeNode(token = "root", maxChild = maxChild).add(newLogLine, newLogLine.template.toList, 1)
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
          seqTrees = seqTrees.updated(templateLength, tree.remove(newLogLine, newLogLine.template.toList, 1))
        )
      case None =>
        this
    }
  }

  def search(tokenSeq: TokenSeq, similarityThreshold: Double = 0.4): Option[LogLineType] = {
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
                          token: String = "",
                          depth: Int = 1,
                          maxChild: Int = 100) {

  def add(newLogLine: LogLineType): PrefixTreeNode = add(newLogLine, newLogLine.template.toList, 1)

  def add(newLogLine: LogLineType, tokenSeq: List[String], currentDepth: Int): PrefixTreeNode = {
    tokenSeq match {
      case head :: tail =>
        if (currentDepth >= this.depth || currentDepth > newLogLine.template.size) {
          this.copy(
            clusters = this.clusters + newLogLine
          )
        } else {
          (children.get(head), children.get("<*>")) match {
            case (None, Some(wildcard)) if hasNumbers(token) =>
              this.copy(
                children = children.updated("<*>", wildcard.add(newLogLine, tail, currentDepth + 1))
              )

            case (None, None) if hasNumbers(token) =>
              val newNode =
                PrefixTreeNode(token = "<*>", maxChild = maxChild, depth = currentDepth + 1)
                  .add(newLogLine, tail, currentDepth + 1)
              this.copy(
                children = children.updated("<*>", newNode)
              )

            case (None, Some(wildcard)) if !hasNumbers(token) =>
              if (this.children.size < maxChild) {
                val newNode =
                  PrefixTreeNode(token = token, maxChild = maxChild, depth = currentDepth + 1)
                    .add(newLogLine, tail, currentDepth + 1)
                this.copy(
                  children = children.updated(token, newNode)
                )
              } else {
                this.copy(
                  children = children.updated("<*>", wildcard.add(newLogLine, tail, currentDepth + 1))
                )
              }

            case (None, None) if !hasNumbers(token) =>
              if (this.children.size + 1 < maxChild) {
                val newNode =
                  PrefixTreeNode(token = token, maxChild = maxChild, depth = currentDepth + 1)
                    .add(newLogLine, tail, currentDepth + 1)
                this.copy(
                  children = children.updated(token, newNode)
                )

              } else if(this.children.size + 1 == maxChild) {
                val newNode =
                  PrefixTreeNode(token = "<*>", maxChild = maxChild, depth = currentDepth + 1)
                    .add(newLogLine, tail, currentDepth + 1)
                this.copy(
                  children = children.updated("<*>", newNode)
                )

              } else {
                ???
//                this.copy(
//                  children = children.updated(head, child.add(newLogLine, tail, currentDepth + 1))
//                )
              }

            case (Some(child), _) =>
              this.copy(
                children = children.updated(head, child.add(newLogLine, tail, currentDepth + 1))
              )


          }
        }
      case Nil => ???
    }
  }

  def remove(newLogLine: LogLineType): PrefixTreeNode = remove(newLogLine, newLogLine.template.toList, 1)

  def remove(newLogLine: LogLineType, tokenSeq: List[String], currentDepth: Int): PrefixTreeNode = {
    tokenSeq match {
      case head :: tail =>
        if (currentDepth >= this.depth || currentDepth > newLogLine.template.size) {
          this.copy(
            clusters = this.clusters - newLogLine
          )
        } else {
          (children.get(head), children.get("<*>")) match {
            case (Some(child), Some(wildcard)) =>
              this.copy(
                children =
                  children
                    .updated(head, child.remove(newLogLine, tail, currentDepth + 1))
                    .updated("<*>", wildcard.remove(newLogLine, tail, currentDepth + 1))
              )

            case (None, Some(wildcard)) =>
              this.copy(
                children = children.updated("<*>", wildcard.remove(newLogLine, tail, currentDepth + 1))
              )

            case (_, _) => this
          }
        }
      case Nil => ???
    }
  }

  def search(tokenSeq: TokenSeq): Option[Set[LogLineType]] = this.search(tokenSeq.toList, 1, tokenSeq.size, None)

  def search(tokenSeq: List[String], currentDepth: Int, seqLen: Int, currentMatch: Option[Set[LogLineType]]): Option[Set[LogLineType]] = {
    if (currentDepth >= this.depth || currentDepth > seqLen) {
      currentMatch.orElse(Some(this.clusters))
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