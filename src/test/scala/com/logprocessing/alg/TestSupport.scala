package com.logprocessing.alg

import java.util.UUID

import com.logprocessing.log.{LogLineType, TokenSeq}

import scala.util.Random

object TestSupport {

  def clusters(logLineTypes: LogLineType*): Set[LogLineType] = logLineTypes.toSet

  def line(seqStr: String, id: UUID = UUID.randomUUID()): LogLineType = LogLineType(seq(seqStr), id)

  def list(seqStr: String): List[String] = seq(seqStr).toList

  def seq(seqStr: String): TokenSeq = seqStr.map(_.toString)

  def generate(size: Int, seqMin: Int, seqMax: Int): IndexedSeq[LogLineType] = {
    (0 until size).map { _ =>
      Random.alphanumeric
        .take(Random.nextInt(seqMax) + seqMin)
        .map(_.toString)
        .foldLeft(IndexedSeq.empty[String])(_ :+ _)
    }
    .distinct
    .map(LogLineType(_))
  }

}
