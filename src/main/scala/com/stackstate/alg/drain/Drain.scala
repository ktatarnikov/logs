package com.stackstate.alg.drain

import com.stackstate.alg.LogSplitter.{LogLine, LogLineSplitter}
import com.stackstate.alg.log.EventType
import com.stackstate.alg.spell.Spell

case class Drain(splitter: LogLineSplitter) {

  def parse(text: String): (Drain, EventType, LogLine) = {
    ???
  }
}
