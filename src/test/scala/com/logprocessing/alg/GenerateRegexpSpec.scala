package com.logprocessing.alg

import com.logprocessing.alg.LogSplitter._
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class GenerateRegexpSpec extends WordSpec {

  "generateRegexp" should {
    "handle empty pattern" in {
      val (headers, regexp) = generateRegexp("")
      headers shouldBe Nil
      regexp.pattern.pattern() shouldBe "^$"
    }

    "handle one field pattern" in {
      val (headers, regexp) = generateRegexp("<Date>")
      headers shouldBe "Date" :: Nil
      regexp.pattern.pattern() shouldBe "^(?<Date>.*?)$"
    }

    "handle two field pattern" in {
      val (headers, regexp) = generateRegexp("1<Date> 2 <Time> 3")
      headers shouldBe "Date" :: "Time" :: Nil
      regexp.pattern.pattern() shouldBe "^1(?<Date>.*?)\\s+2 (?<Time>.*?)\\s+3$"
    }

    "handle multiple field pattern" in {
      val (headers, regexp) = generateRegexp("<Date> <Time> <Pid> <Level> <Component>: <Content>")
      headers shouldBe "Date" :: "Time" :: "Pid" :: "Level" :: "Component" :: "Content" :: Nil
      regexp.pattern.pattern() shouldBe "^(?<Date>.*?)\\s+(?<Time>.*?)\\s+(?<Pid>.*?)\\s+(?<Level>.*?)\\s+(?<Component>.*?):\\s+(?<Content>.*?)$"
    }

  }
}
