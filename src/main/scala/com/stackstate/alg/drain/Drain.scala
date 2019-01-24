package com.stackstate.alg.drain

import com.stackstate.alg.LogSplitter.{LogLine, LogLineSplitter}
import com.stackstate.alg.drain.Drain.getTemplate
import com.stackstate.alg.log.{EventType, LogLineType, TokenSeq}


object Drain {
  def getTemplate(sequence1: TokenSeq, sequence2: TokenSeq): TokenSeq = {
    if (sequence1.size != sequence2.size) {
      throw new RuntimeException(s"Sequences $sequence1 and $sequence2 are not alinged.")
    }

    def recourse(seq1: List[String], seq2: List[String]): List[String] = {
      (seq1, seq2) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (h1 :: t1, h2 :: t2) if h1 == h2 => h1 :: recourse(t1, t2)
        case (h1 :: t1, h2 :: t2) if h1 != h2 => "*" :: recourse(t1, t2)
      }
    }

    recourse(sequence1.toList, sequence2.toList).toIndexedSeq
  }

}


case class Drain(splitter: LogLineSplitter,
                 tree: PrefixTreeNode = PrefixTreeNode(),
                 depth: Int = 4,
                 similarityThreshold : Double = 0.4,
                 maxChild: Int = 100) {

  def parse(text: String): (Drain, EventType, LogLine) = {
    val logLine = splitter.split(text)
    val tokenSeq = logLine.contents
    tree.search(tokenSeq, similarityThreshold) match {
      case Some(logLineType) =>
        val templateSeq = getTemplate(tokenSeq, logLineType.template)
        if (templateSeq != logLineType.template) {
          val newLogLine = logLineType.copy(template = templateSeq)
          val updatedTree = tree.remove(logLineType)
          (
            this.copy(
              tree = updatedTree.add(newLogLine)
            ),
            newLogLine.eventType,
            logLine
          )
        } else {
          (
            this,
            logLineType.eventType,
            logLine
          )
        }
      case None =>
        val newLogLineType = LogLineType(template = tokenSeq)
        (
          this.copy(
            tree = tree.add(newLogLineType)
          ),
          newLogLineType.eventType,
          logLine
        )
    }
  }
}
