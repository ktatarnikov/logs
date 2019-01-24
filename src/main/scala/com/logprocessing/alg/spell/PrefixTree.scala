package com.logprocessing.alg.spell

import com.logprocessing.log._

object PrefixTree {

  case class PrefixTreeNode(children: Map[Token, PrefixTreeNode] = Map(), logNum: Int = 0, logLineType: Option[LogLineType] = None) {

    def add(logLineType: LogLineType): PrefixTreeNode = this.add(logLineType, logLineType.templateWithoutWildCards.toList)

    private def add(logLineType: LogLineType, seq: List[Token]): PrefixTreeNode = {

      (seq, children.get(seq.head)) match {
        case (head :: Nil, Some(child)) =>
          val newChild = child.addLeaf(logLineType)
          this.copy(
            children = children.updated(head, newChild),
            logNum = logNum + 1,
          )

        case (head :: Nil, None) =>
          this.copy(
            children = children.updated(head, PrefixTreeNode().addLeaf(logLineType)),
            logNum = logNum + 1,
          )

        case (head :: tail, Some(child)) =>
          this.copy(
            children = children.updated(head, child.add(logLineType, tail)),
            logNum = logNum + 1
          )

        case (head :: tail, None) =>
          val newNode = PrefixTreeNode().add(logLineType, tail)
          this.copy(
            children = children.updated(head, newNode),
            logNum = logNum + 1
          )
      }
    }

    private def addLeaf(logLineType: LogLineType): PrefixTreeNode = {
      this.copy(
        logLineType = this.logLineType.orElse(Some(logLineType)),
        logNum = this.logNum + 1
      )
    }


    def remove(logLineType: LogLineType): PrefixTreeNode = this.remove(logLineType, logLineType.templateWithoutWildCards.toList)

    private def remove(logLineType: LogLineType, seq: List[String]): PrefixTreeNode = {
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

    private def removeLeaf(logLineType: LogLineType): PrefixTreeNode = {
      if (this.logLineType.contains(logLineType)) {
        this.copy(
          logLineType = None,
          logNum = logNum - 1
        )
      } else {
        this.copy(logNum = logNum - 1)
      }
    }

    def matchSeq(seq: TokenSeq, tau: Double = 0.0): Option[LogLineType] = this.matchSeq(seq.toList, seq.size, tau)

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

}
