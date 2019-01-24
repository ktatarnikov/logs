package com.stackstate.alg.drain

import com.stackstate.alg.log.{LogLineType, Token, TokenSeq}


object PrefixTree {
  case class PrefixTreeNode(children: Map[Token, PrefixTreeNode] = Map(), digitOrToken: String = "", depth: Int = 0) {
    def add(newLogLine: LogLineType): PrefixTreeNode = {
      this
    }
    def remove(newLogLine: LogLineType): PrefixTreeNode = {
      this
    }

    def search(tokenSeq: TokenSeq): Option[LogLineType] = {
      None
    }

  }
}
