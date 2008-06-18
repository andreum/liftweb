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

import scala.actors._
import scala.actors.Actor._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import scala.collection.immutable.TreeMap
import scala.xml._
import S._
import SHtml._
import net.liftweb.util._
import net.liftweb.http.js._
import JsCmds._


class Chat(initInfo: CometActorInitInfo) extends CometActor(initInfo) {
  private var userName = ""
  private var currentData: List[ChatLine] = Nil
  def defaultPrefix = "chat"

  private lazy val infoId = uniqueId + "_info"

  private val server = {
    val ret = ChatServer
    ret ! ChatServerAdd(this)
    ret
  }

  override def lowPriority = {
    case ChatServerUpdate(value) =>
    (value diff currentData) match {
      case Nil =>
      case diff => partialUpdate(diff.reverse.foldLeft(Noop)((a,b) => a & AppendHtml(infoId, line(b))))
    }

    currentData = value
  }

  override lazy val fixedRender: Can[NodeSeq] = {
    val n = "id"+randomString(10)
    ajaxForm(After(100, SetValueAndFocus(n, "")),
    (text("", sendMessage _) % ("id" -> n)) ++ <input type="submit" value="Chat"/> )
  }

  def line(cl: ChatLine) = (<li>{hourFormat(cl.when)} {cl.user}: {cl.msg}</li>)

  override def render = (<span>Hello "{userName}"
  <ul id={infoId}>{currentData.reverse.flatMap(line)}</ul>
  </span>)

  override def localSetup {
    if (userName.length == 0) {
      ask(new AskName(CometActorInitInfo(theSession, name, defaultXml, attributes)), "what's your username") {
        case s: String if (s.trim.length > 2) => userName = s.trim; reRender(true)
        case s => localSetup; reRender(false)
      }
    }
  }

  // def waitForUpdate : Option[List[ChatLine]] = receiveWithin(100) {case ChatServerUpdate(l) => Some(l) case TIMEOUT => None}

  def sendMessage(msg: String) = server ! ChatServerMsg(userName, msg.trim)


}
