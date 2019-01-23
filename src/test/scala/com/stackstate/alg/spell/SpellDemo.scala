package com.stackstate.alg.spell

import java.io.{File, PrintWriter}

import com.stackstate.alg.logs.LogSplitter
import com.stackstate.alg.logs.LogSplitter.LogLine
import com.stackstate.alg.spell.Spell.EventType

import scala.io.Source

object SpellDemo extends App {
  val logFormat = "<Date> <Time> <Pid> <Level> <Component>: <Content>"
  val logSplitter = new LogSplitter(logFormat)
  val spell = new Spell(logSplitter)
  val stream = ClassLoader.getSystemResourceAsStream("HDFS_2k.log")
  val logLines = Source.fromInputStream(stream).getLines.toList

  case class LoglineAndType(logline: LogLine, eventType: EventType)
  case class ParseResult(spell: Spell, typed: IndexedSeq[LoglineAndType])

  val result =
    logLines.foldLeft(ParseResult(spell, IndexedSeq.empty)) { (acc, line) =>
      val (spell, eventType, logLine) = acc.spell.parse(line)
      acc.copy(
        spell,
        acc.typed :+ LoglineAndType(logLine, eventType)
      )
    }

  val marked = new PrintWriter(new File("./HDFS_2k.marked.log"))
  result.typed.foreach(row => marked.write(s"${row.eventType} ${row.logline.contents.mkString(" ")}\n"))
  marked.close()


  val clusters = new PrintWriter(new File("./HDFS_2k.clusters.log"))
  result.spell.clusters.foreach(row => clusters.write(s"${row.eventType} ${row.template.mkString(" ")}\n"))
  clusters.close()

}