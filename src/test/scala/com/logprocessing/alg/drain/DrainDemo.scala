package com.logprocessing.alg.drain

import com.logprocessing.alg.DemoAppBase
import com.logprocessing.log.LogLineType

object DrainDemo extends DemoAppBase {

  case class ParseResult(drain: Drain, typed: IndexedSeq[LineAndType])

  override def name = "drain"
  override def preprocessRegexp =
    Set(
      "blk_(|-)[0-9]+",
      "(/|)([0-9]+\\.){3}[0-9]+(:[0-9]+|)(:|)",
      "(?<=[^A-Za-z0-9])(\\-?\\+?\\d+)(?=[^A-Za-z0-9])|[0-9]+$"
    )
  override def execute(lines: Seq[String]): (Set[LogLineType], IndexedSeq[LineAndType]) = {
    val result =
      logLines.foldLeft(ParseResult(new Drain(logSplitter, similarityThreshold = 0.5, depth = 4), IndexedSeq.empty)) { (acc, line) =>
        val (drain, eventType, logLine) = acc.drain.parse(line)
        acc.copy(
          drain,
          acc.typed :+ LineAndType(logLine, eventType)
        )
      }
    (result.drain.clusters, result.typed)
  }

}
