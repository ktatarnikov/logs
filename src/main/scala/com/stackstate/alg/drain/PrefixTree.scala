package com.stackstate.alg.drain

import com.stackstate.alg.log.{LogLineType, Token}


object PrefixTree {
  case class PrefixTreeNode(children: Map[Token, PrefixTreeNode] = Map(), logNum: Int = 0, logLineType: Option[LogLineType] = None) {

  }
}
