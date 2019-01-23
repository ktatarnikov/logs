package com.stackstate.alg.logs

import com.stackstate.alg.logs.LogSplitter.{LogLine, LogLineSplitter, generateRegexp, splitIntoTokens}
import com.stackstate.alg.spell.Spell.TokenSeq

import scala.util.matching.Regex

object LogSplitter {
  trait LogLineSplitter {
    def split(logLine: String): LogLine
  }

  case class LogLine(headers: Map[String, String], contents: TokenSeq)

  def generateRegexp(format: String): (List[String], Regex) = {
    var prev = 0

    val splitters =
      "(<[^<>]+>)".r.findAllMatchIn(format)
        .foldLeft(IndexedSeq.empty[String]) { (acc, m) =>
          val prevGroup = format.substring(prev, m.start(0))
          prev = m.end(0)
          acc :+ prevGroup :+ m.group(0)
        } :+ format.substring(prev, format.length)

    var regex = ""
    var headers = List.empty[String]
    for (k <- splitters.indices) {
      if (k % 2 == 0) {
        val splitter = " +".r.replaceFirstIn(splitters(k), "\\\\s+")
        regex += splitter
      } else {
        val header = splitters(k).replace("<", "").replace(">", "")
        regex += s"(?<$header>.*?)"
        headers = headers ::: header :: Nil
      }
    }

    val result = s"^${regex}$$".r
    (headers, result)
  }

  val TokenizeContentRegExp = "[\\s=:,]".r
  def splitIntoTokens(text: String): TokenSeq = {
    TokenizeContentRegExp.split(text).filter(!_.isEmpty).toIndexedSeq
  }
}

class LogSplitter(format: String, contentsGroup: String = "Content") extends LogLineSplitter {
  val (headers, regex) = generateRegexp(format)
  override def split(logLine: String): LogLine = {
    regex.findAllMatchIn(logLine).map(m => {
      headers.foldLeft(LogLine(Map.empty, IndexedSeq())) { (acc, groupName) => {

          val contents =
            if (groupName == contentsGroup)
              Some(splitIntoTokens(m.group(groupName)))
            else
              None

          acc.copy(
            acc.headers + (groupName -> m.group(groupName)),
            contents.getOrElse(acc.contents)
          )
        }
      }
    }).next()
  }
}
