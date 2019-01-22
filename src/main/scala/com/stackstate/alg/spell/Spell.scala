package com.stackstate.alg.spell

import java.util.UUID

import com.stackstate.alg.spell.Spell.{TokenSeq, _}

import scala.annotation.tailrec

object Spell {

  type Token = String
  type TokenSeq = IndexedSeq[Token]
  type EventType = UUID

  trait Tree {
    def add(logLineType: LogLineType, seq: List[String]): Tree

    def remove(logLineType: LogLineType, seq: List[String]): Tree

    def matchSeq(seq: List[String], tau: Double): Option[LogLineType]

    def logNum: Int
  }

  case class Leaf() extends Tree {
    override def add(logLineType: LogLineType, seq: List[String]): Tree = this

    override def remove(logLineType: LogLineType, seq: List[String]): Tree = this

    override def matchSeq(seq: List[String], tau: Double): Option[LogLineType] = None

    override def logNum: Int = 0
  }

  case class Node(trees: Map[Token, Tree] = Map(), logNum: Int = 0, logLineType: Option[LogLineType] = None) extends Tree {
    // TODO remove wildcards from seq
    override def add(logLineType: LogLineType, seq: List[Token]): Tree = {
      (seq, trees.get(seq.head)) match {
        case (head :: tail, Some(template)) =>
          this.copy(
            trees = trees.updated(head, template.add(logLineType, tail)),
            logNum = logNum + 1
          )
        case (head :: tail, None) =>
          val newNode = Node().add(logLineType, tail)
          this.copy(
            trees = trees.updated(head, Node(Map(head -> newNode)))
          )
        case (Nil, _) => Leaf()
      }
    }

    // TODO remove wildcards from seq
    override def remove(logLineType: LogLineType, seq: List[String]): Tree = {
      (seq, trees.get(seq.head)) match {
        case (head :: tail, Some(template)) =>
          val withRemoved = template.remove(logLineType, tail)
          if (withRemoved.logNum == 0) {
            this.copy(
              trees = trees - head,
              logNum = logNum - 1
            )
          } else {
            this.copy(
              trees = trees.updated(head, withRemoved),
              logNum = logNum - 1
            )
          }
        case _ => this
      }
    }

    override def matchSeq(seq: List[String], tau: Double): Option[LogLineType] = {
      (seq, trees.get(seq.head)) match {
        case (head :: Nil, Some(template)) => this.logLineType
        case (head :: tail, Some(template)) =>
          logLineType match {
            case Some(theType) =>
              if (theType.templateWithoutWildCards.size >= tau * seq.size) {
                Some(theType)
              } else {
                template.matchSeq(tail, tau)
              }
            case _ => template.matchSeq(tail, tau)
          }
        case (head :: tail, None) => None
        case (Nil, _) => None
      }
    }
  }

  case class LogLineType(template: TokenSeq, eventType: EventType = UUID.randomUUID()) {
    def templateWithoutWildCards: TokenSeq = template.filter(_ != "*")
  }

  trait LogLineSplitter {
    def split(logLine: String): TokenSeq
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
          case (h :: t, lcsSeq) if h == lcsSeq.head => h :: recourse(t, lcs.tail)
          case (_ :: t, lcsSeq) => "*" :: recourse(t, lcsSeq)
        }
      }

      recourse(seq.toList, lcs.toList).toIndexedSeq
    }
  }

}

//case class EventTypes(clusters: Map[UUID, LogLineType] = Map.empty, prefixTree: Tree = Node()) {}

case class Spell(splitter: LogLineSplitter,
                 tau: Double = 0.5f,
                 clusters: Map[UUID, LogLineType] = Map.empty,
                 prefixTree: Tree = Node()) {

  def parse(logLine: String): (Spell, EventType) = {
    val tokenSeq = splitter.split(logLine)
    val constantSeq = tokenSeq.flatMap {
      case "*" => Seq.empty
      case other => Seq(other)
    }
    matchPrefixTree(constantSeq)
      .orElse(matchTypeDirect(constantSeq))
      .orElse(matchLcs(constantSeq))
      .fold {
        val newType = LogLineType(tokenSeq)

        (
          this.copy(
            clusters = clusters + (newType.eventType -> newType),
            prefixTree = prefixTree.add(newType, newType.template.toList)
          ),
          newType.eventType
        )
      } { logLineType =>
        val newTemplate = getTemplate(lcs(tokenSeq, logLineType.template), logLineType.template)
        if (newTemplate.equals(logLineType.template)) {
          val newLogline = logLineType.copy(newTemplate)

          (
            this.copy(
              clusters = clusters + (newLogline.eventType -> newLogline),
              prefixTree = prefixTree.remove(logLineType, logLineType.template.toList)
                .add(newLogline, newLogline.template.toList)
            ),
            newLogline.eventType
          )
        } else
          (this, logLineType.eventType)
      }
  }

  def matchTypeDirect(constantSeq: TokenSeq): Option[LogLineType] = {
    val seq = constantSeq.toSet

    def matchCluster(logLineType: LogLineType): Boolean = {
      logLineType.template.size >= 0.5 * seq.size &&
        logLineType.template.forall(token => seq.contains(token) || "*".equals(token))
    }

    this.clusters.values.find(matchCluster)
  }

  def matchPrefixTree(constantSeq: TokenSeq): Option[LogLineType] = {
    prefixTree.matchSeq(constantSeq.toList, this.tau)
  }

  def matchLcs(constantSeq: TokenSeq): Option[LogLineType] = {
    val seq = constantSeq.toSet

    val maxCluster =
      this.clusters.values.foldLeft(Option.empty[LogLineType]) { (currentMax, logLineType) =>
        val template = logLineType.template.toSet
        if (template.size < 0.5 * seq.size) {
          currentMax
        } else {
          val lcsSeq = lcs(constantSeq, logLineType.template)
          val currentMaxLength = currentMax.map(_.template.size).getOrElse(0)
          val lcsMoreThanCurrentMax = lcsSeq.size > currentMaxLength
          val smallerTemplate = lcsSeq.size == currentMaxLength && logLineType.template.size < currentMaxLength
          if (lcsMoreThanCurrentMax || smallerTemplate)
            Some(logLineType)
          else
            currentMax
        }
      }
    maxCluster.flatMap { logLineType =>
      val lcsSeq = lcs(constantSeq, logLineType.template)
      if (lcsSeq.size > this.tau * seq.size)
        Some(logLineType)
      else
        None
    }
  }

}
