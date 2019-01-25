package com.logprocessing.alg

import LogSplitter._
import com.logprocessing.log._

import scala.util.matching.Regex

object LogSplitter {

  val TokenizeContentRegExp = "[\\s=:,]".r

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

  def splitIntoTokens(text: String): TokenSeq = {
    TokenizeContentRegExp.split(text).filter(!_.isEmpty).toIndexedSeq
  }

  trait LogLineSplitter {
    def split(logLine: String): LogLine
  }

  case class LogLine(headers: Map[String, String], contents: TokenSeq)
}

class LogSplitter(format: String,
                  regexps: Set[String] = Set.empty,
                  contentsGroup: String = "Content")
  extends LogLineSplitter {
  val preprocessRegexps = regexps.map(_.r)
  val (headers, regex) = generateRegexp(format)

  def preprocess(line: String) : String = {
    preprocessRegexps.foldLeft(line) { case (line, regexp) =>
      regexp.replaceAllIn(line, "<*>")
    }
  }

  override def split(logLine: String): LogLine = {

    val preprocessed = preprocess(logLine)

    regex.findAllMatchIn(preprocessed).map(m => {
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
