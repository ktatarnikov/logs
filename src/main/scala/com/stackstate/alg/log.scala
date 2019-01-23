package com.stackstate.alg

import java.util.UUID

package object log {

  type Token = String
  type TokenSeq = IndexedSeq[Token]
  type EventType = UUID

  case class LogLineType(template: TokenSeq, eventType: EventType = UUID.randomUUID()) {
    def templateWithoutWildCards: TokenSeq = template.filter(_ != "*")
  }

}
