package com.logprocessing.alg

import java.io.{File, PrintWriter}

import com.logprocessing.alg.LogSplitter.LogLine
import com.logprocessing.log.{EventType, LogLineType}

import scala.io.Source

trait DemoAppBase extends App {

  case class LineAndType(logLine: LogLine, eventType: EventType)

  def name : String
  def execute(lines: Seq[String]): (Set[LogLineType], IndexedSeq[LineAndType])

  val logFormat = "<Date> <Time> <Pid> <Level> <Component>: <Content>"
  val logSplitter = new LogSplitter(logFormat)

  val stream = ClassLoader.getSystemResourceAsStream("HDFS_2k.log")
  val logLines = Source.fromInputStream(stream).getLines.toList

  val (resultTypes, resultLines) = execute(logLines)

  val marked = new PrintWriter(new File(s"./HDFS_2k.${name}.marked.log"))
  val clusters = new PrintWriter(new File(s"./HDFS_2k.${name}.templates.log"))

  resultLines.foreach(row => marked.write(s"${row.eventType} ${row.logLine.contents.mkString(" ")}\n"))
  marked.close()

  resultTypes.foreach(row => clusters.write(s"${row.eventType} ${row.template.mkString(" ")}\n"))
  clusters.close()

}
