package com.stackstate.alg.spell

import com.stackstate.alg.logs.LogSplitter.{LogLine, LogLineSplitter}
import com.stackstate.alg.spell.Spell._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class SpellSpec extends WordSpec {

  val simpleSplitter: LogLineSplitter = (logLine: String) => LogLine(Map.empty, logLine.split(" "))
  var spell = new SpellWrapper(Spell(simpleSplitter))

  "spell" should {
    "create log line types" in {
      val eventType1 = spell.parse("A B C D")
      val eventType2 = spell.parse("E F G H")
      val eventType3 = spell.parse("X Y Z")
      eventType2 shouldNot be(eventType1)
      eventType3 shouldNot be(eventType1)
      eventType3 shouldNot be(eventType2)

      spell.parse("A B C D") shouldBe eventType1
      spell.parse("A B C") shouldBe eventType1
      spell.parse("A B") shouldBe eventType1

      spell.parse("E F G H") shouldBe eventType2
      spell.parse("E F G") shouldBe eventType2
      spell.parse("E F") shouldBe eventType2

      spell.parse("X Y Z") shouldBe eventType3
      spell.parse("X Y") shouldBe eventType3
    }
  }

  class SpellWrapper(var spell: Spell) {
    def parse(line: String): EventType = {
      var (next: Spell, eventType, logLine) = spell.parse(line)
      spell = next
      eventType
    }
  }
}
