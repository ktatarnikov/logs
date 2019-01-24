package com.logprocessing.alg

import org.scalatest.Matchers._
import org.scalatest.WordSpec

import scala.io.Source

class LogSplitterSpec extends WordSpec {

  "LogSplitter" should {
    "handle split the log line according to the pattern." in {
      val splitter = new LogSplitter("<Date> <Time> <Pid> <Level> <Component>: <Content>")
      val logLine =
        splitter.split("081109 204106 329 INFO dfs.DataNode$PacketResponder: PacketResponder 2 for block blk_-6670958622368987959 terminating")
      logLine.headers shouldBe Map("Date" -> "081109", "Time" -> "204106", "Pid" -> "329", "Level" -> "INFO", "Component" -> "dfs.DataNode$PacketResponder", "Content" -> "PacketResponder 2 for block blk_-6670958622368987959 terminating")
      logLine.contents shouldBe IndexedSeq("PacketResponder", "2", "for", "block", "blk_-6670958622368987959", "terminating")
    }


    "handle entire file." in {
      val splitter = new LogSplitter("<Date> <Time> <Pid> <Level> <Component>: <Content>")
      val logLines = Source.fromInputStream(ClassLoader.getSystemResourceAsStream("HDFS_2k.log")).getLines.map(splitter.split).toList
      logLines.forall(_.headers.size == 6) shouldBe true
      logLines.forall(_.headers.values.forall(!_.isEmpty)) shouldBe true
      logLines.forall(_.contents.forall(!_.isEmpty)) shouldBe true
      logLines.forall(_.contents.size >= 3) shouldBe true
    }

  }

}
