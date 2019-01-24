package com.logprocessing.alg.spell

import com.logprocessing.alg.DemoAppBase
import com.logprocessing.log._

object SpellDemo extends DemoAppBase {
  case class ParseResult(spell: Spell, typed: IndexedSeq[LineAndType])

  override def name = "spell"

  override def execute(lines: Seq[String]): (Set[LogLineType], IndexedSeq[LineAndType]) = {
    val result =
      logLines.foldLeft(ParseResult(new Spell(logSplitter), IndexedSeq.empty)) { (acc, line) =>
        val (drain, eventType, logLine) = acc.spell.parse(line)
        acc.copy(
          drain,
          acc.typed :+ LineAndType(logLine, eventType)
        )
      }
    (result.spell.clusters, result.typed)
  }

}
