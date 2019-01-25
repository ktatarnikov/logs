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
      val eventType1 = drain.parse("A B C D E F G")
      val eventType2 = drain.parse("H I J K L M N")
      val eventType3 = drain.parse("O P Q R S")
      val eventType4 = drain.parse("<*> <*> <*> <*> <*> <*> G")
      eventType2 shouldNot be(eventType1)
      eventType3 shouldNot be(eventType1)
      eventType3 shouldNot be(eventType2)

      drain.parse("A B C D E F G") shouldBe eventType1
      drain.parse("A B C M D E F") shouldBe eventType1
      drain.parse("A B C 3 4 5 6") shouldBe eventType1
      drain.parse("A B C D E F") shouldNot be(eventType1)
      drain.parse("A B C D E 1") shouldNot be(eventType1)
      drain.parse("A B C <*> <*> <*> G") shouldBe eventType1

      drain.parse("<*> <*> <*> <*> <*> <*> G") shouldBe eventType4
      drain.parse("<*> <*> <*> <*> <*> F G") shouldNot be(eventType4)

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
