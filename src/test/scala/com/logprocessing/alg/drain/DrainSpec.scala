package com.logprocessing.alg.drain

import com.logprocessing.alg.LogSplitter.LogLineSplitter
import com.logprocessing.alg.LogSplitter._
import org.scalatest.Matchers._
import org.scalatest.WordSpec
import com.logprocessing.log._

class DrainSpec extends WordSpec {

  val simpleSplitter: LogLineSplitter = (logLine: String) => LogLine(Map.empty, logLine.split(" "))
  var drain = new DrainWrapper(Drain(simpleSplitter))

  "drain" should {
    "create log line types" in {
      val eventType1 = drain.parse("A B C D")
      val eventType2 = drain.parse("E F G H")
      val eventType3 = drain.parse("X Y Z")
      eventType2 shouldNot be(eventType1)
      eventType3 shouldNot be(eventType1)
      eventType3 shouldNot be(eventType2)

      drain.parse("A B C D") shouldBe eventType1
      drain.parse("A B C") shouldBe eventType1
      drain.parse("A B") shouldBe eventType1

      drain.parse("E F G H") shouldBe eventType2
      drain.parse("E F G") shouldBe eventType2
      drain.parse("E F") shouldBe eventType2

      drain.parse("X Y Z") shouldBe eventType3
      drain.parse("X Y") shouldBe eventType3
    }
  }

  class DrainWrapper(var spell: Drain) {
    def parse(line: String): EventType = {
      var (next: Drain, eventType, logLine) = spell.parse(line)
      spell = next
      eventType
    }
  }

}
