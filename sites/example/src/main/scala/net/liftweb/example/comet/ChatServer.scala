/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.example.comet

import _root_.scala.actors.Actor
import _root_.scala.actors.Actor._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.scala.xml.{NodeSeq}
import _root_.scala.collection.immutable.TreeMap
import _root_.net.liftweb.textile.TextileParser
import _root_.scala.xml.Text
import _root_.java.util.Date

/**
* A chat server.  It gets messages and returns them
*/

object ChatServer extends Actor {
  private var chats: List[ChatLine] = Nil
  private var listeners: List[Actor] = Nil

  def act = loop {
    react {
      case ChatServerMsg(user, msg) if msg.length > 0 =>
      chats ::= ChatLine(user, toHtml(msg), timeNow)
      chats = chats.take(50)
      val toDistribute = chats.take(15)
      listeners.foreach (_ ! ChatServerUpdate(toDistribute))

      case ChatServerAdd(me) =>
      me ! ChatServerUpdate(chats.take(15))
      listeners ::= me


      case ChatServerRemove(me) =>
      listeners -= me

      case _ =>
    }
  }

  /**
  * Convert an incoming string into XHTML using Textile Markup
  *
  * @param msg the incoming string
  *
  * @return textile markup for the incoming string
  */
  def toHtml(msg: String): NodeSeq = TextileParser.parse(msg, Empty). // parse it
  map(_.toHtml.toList match {case Nil => Nil case x :: xs => x.child}).  // convert to html and get the first child (to avoid things being wrapped in <p>)
  getOrElse(Text(msg)) // if it wasn't parsable, then just return a Text node of the message

  this.start
}

case class ChatLine(user: String, msg: NodeSeq, when: Date)
case class ChatServerMsg(user: String, msg: String)
case class ChatServerUpdate(msgs: List[ChatLine])
case class ChatServerAdd(me: Actor)
case class ChatServerRemove(me: Actor)

