package com.stackstate.alg.spell

import java.util.UUID

import com.stackstate.alg.logs.LogSplitter.{LogLine, LogLineSplitter}
import com.stackstate.alg.spell.Spell.{LogLineType, TokenSeq, _}

import scala.annotation.tailrec
import scala.collection.immutable.{ListMap, TreeMap}

object Spell {

  type Token = String
  type TokenSeq = IndexedSeq[Token]
  type EventType = UUID

  case class Node(children: Map[Token, Node] = Map(), logNum: Int = 0, logLineType: Option[LogLineType] = None) {

    def addLeaf(logLineType: LogLineType) : Node = {
      this.copy(
        logLineType = this.logLineType.orElse(Some(logLineType)),
        logNum = this.logNum + 1
      )
    }

    def removeLeaf(logLineType: LogLineType) : Node = {
      if (this.logLineType.contains(logLineType)) {
        this.copy(
          logLineType = None,
          logNum = logNum - 1
        )
      } else {
        this.copy(logNum = logNum - 1)
      }
    }

    def add(logLineType: LogLineType): Node = this.add(logLineType, logLineType.templateWithoutWildCards.toList)
    def remove(logLineType: LogLineType): Node = this.remove(logLineType, logLineType.templateWithoutWildCards.toList)
    def matchSeq(seq: TokenSeq, tau: Double = 0.0): Option[LogLineType] = this.matchSeq(seq.toList, seq.size, tau)

    private def add(logLineType: LogLineType, seq: List[Token]): Node = {

      (seq, children.get(seq.head)) match {
        case (head :: Nil, Some(child)) =>
          val newChild = child.addLeaf(logLineType)
          this.copy(
            children = children.updated(head, newChild),
            logNum = logNum + 1,
          )

        case (head :: Nil, None) =>
          this.copy(
            children = children.updated(head, Node().addLeaf(logLineType)),
            logNum = logNum + 1,
          )

        case (head :: tail, Some(child)) =>
          this.copy(
            children = children.updated(head, child.add(logLineType, tail)),
            logNum = logNum + 1
          )

        case (head :: tail, None) =>
          val newNode = Node().add(logLineType, tail)
          this.copy(
            children = children.updated(head, newNode),
            logNum = logNum + 1
          )
      }
    }

    private def remove(logLineType: LogLineType, seq: List[String]): Node = {
      (seq, children.get(seq.head)) match {
        case (head :: Nil, Some(template)) =>
          val newLeaf = template.removeLeaf(logLineType)
          if (newLeaf.logNum == 0) {
            this.copy(
              children = children - head,
              logNum = logNum - 1
            )
          } else {
            this.copy(
              children = children.updated(head, newLeaf),
              logNum = logNum - 1
            )
          }
        case (head :: tail, Some(template)) =>
          val withRemoved = template.remove(logLineType, tail)
          if (withRemoved.logNum == 0) {
            this.copy(
              children = children - head,
              logNum = logNum - 1
            )
          } else {
            this.copy(
              children = children.updated(head, withRemoved),
              logNum = logNum - 1
            )
          }
        case _ => this
      }
    }

    private def matchSeq(seq: List[String], originalLength: Int, tau: Double): Option[LogLineType] = {
      (seq, children.get(seq.head)) match {
        case (Nil, _) => None
        case (head :: Nil, Some(template)) =>
          template.logLineType match {
            case Some(theType) =>
              if (theType.templateWithoutWildCards.size.toDouble >= tau * originalLength) {
                Some(theType)
              } else {
                None
              }
            case _ => None
          }
        case (head :: Nil, None) => None

        case (head :: tail, Some(template)) =>
          template.logLineType match {
            case Some(theType) =>
              if (theType.templateWithoutWildCards.size.toDouble >= tau * originalLength) {
                Some(theType)
              } else {
                template.matchSeq(tail, originalLength, tau)
              }
            case None => template.matchSeq(tail, originalLength, tau)
          }
        case (head :: tail, None) =>
          matchSeq(tail, originalLength, tau)
      }
    }
  }

  case class LogLineType(template: TokenSeq, eventType: EventType = UUID.randomUUID()) {
    def templateWithoutWildCards: TokenSeq = template.filter(_ != "*")
  }

  def lcs(seq1: TokenSeq, seq2: TokenSeq): TokenSeq = {
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

//case class EventTypes(clusters: Map[UUID, LogLineType] = Map.empty, prefixTree: Tree = Node()) {}

case class Spell(splitter: LogLineSplitter,
                 tau: Double = 0.5f,
                 clusters: IndexedSeq[LogLineType] = IndexedSeq.empty,
                 prefixTree: Node = Node()) {

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
            clusters = clusters :+ newType,
            prefixTree = prefixTree.add(newType)
          ),
          newType.eventType,
          logLine
        )
      } { logLineType =>
        val newTemplate = getTemplate(lcs(tokenSeq, logLineType.template), logLineType.template)
        if (!newTemplate.equals(logLineType.template)) {
          val newLogline = logLineType.copy(newTemplate)

          (
            this.copy(
              clusters = clusters :+ newLogline,
              prefixTree = prefixTree.remove(logLineType).add(newLogline)
            ),
            newLogline.eventType,
            logLine
          )
        } else
          (this, logLineType.eventType, logLine)
      }
  }

  def matchTypeDirect(clusters: IndexedSeq[LogLineType], constantSeq: TokenSeq): Option[LogLineType] = {
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

  def matchLcs(clusters: IndexedSeq[LogLineType], constantSeq: TokenSeq): Option[LogLineType] = {
    val seq = constantSeq.toSet

    case class MaxCluster(logLineType: Option[LogLineType] = None, length: Int = -1)

    val maxCluster =
      clusters.foldLeft(MaxCluster()) { (max, logLineType) =>
        val template = logLineType.template.toSet
        if (template.intersect(seq).size < 0.5 * seq.size) {
          max
        } else {
          val lcsSeq = lcs(constantSeq, logLineType.template)
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
