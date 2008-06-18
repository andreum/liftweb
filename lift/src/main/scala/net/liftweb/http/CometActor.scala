package net.liftweb.http

/*                                                *\
(c) 2007-2008 WorldWide Conferencing, LLC
Distributed under an Apache License
http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.actors.{Actor, Exit}
import scala.actors.Actor._
import scala.collection.mutable.{ListBuffer}
import net.liftweb.util.Helpers._
import net.liftweb.util.{Helpers, Log, Can, Full, Empty, Failure, BindHelpers}
import scala.xml.{NodeSeq, Text, Elem, Unparsed, Node, Group, Null, PrefixedAttribute, UnprefixedAttribute}
import scala.collection.immutable.TreeMap
import scala.collection.mutable.{HashSet, ListBuffer}
import net.liftweb.http.js._
import JsCmds._
import JE._
import java.util.concurrent.atomic.AtomicLong

object CometActor {
  private val serial = new AtomicLong
  
  def next: Long = serial.incrementAndGet
}

object ActorWatcher extends Actor {
  def act = loop {
    react {
      case Exit(actor, why: Throwable) =>
      failureFuncs.foreach(f => tryo(f(actor, why)))
      
      case _ =>
    }
  }
  
  private def startAgain(a: Actor, ignore: Throwable) {a.start}

  private def logActorFailure(actor: Actor, why: Throwable) {
      Log.error("The ActorWatcher restarted "+actor+" because "+why, why)
  }    
  
  /**
  * If there's something to do in addition to starting the actor up, pre-pend the
  * actor to this List
  */
  var failureFuncs: List[(Actor, Throwable) => Unit] = logActorFailure _ :: 
  startAgain _ :: Nil
  
  this.start
  this.trapExit = true
}

@serializable 
abstract class CometActor(val theSession: LiftSession, val name: Can[String], val defaultXml: NodeSeq, val attributes: Map[String, String]) extends Actor with BindHelpers {
  val uniqueId = "LC"+randomString(20)
  private var lastRenderTime = CometActor.next
  private var lastRendering: RenderOut = RenderOut(Full(defaultXml),
  Empty, Empty, Empty, false)
  private var wasLastFullRender = false
  @transient
  private var listeners: List[(ListenerId, AnswerRender => Unit)] = Nil
  private var askingWho: Can[CometActor] = Empty
  private var whosAsking: Can[CometActor] = Empty
  private var answerWith: Can[Any => Any] = Empty
  private var deltas: List[Delta] = Nil
  private var jsonHandlerChain: PartialFunction[Any, JsCmd] = Map.empty
  private val notices = new ListBuffer[(NoticeType.Value, NodeSeq, Can[String])]

  def this(info: CometActorInitInfo) =
  this(info.theSession,info.name,info.defaultXml,info.attributes)
  
  def defaultPrefix: String
  
  /**
  * Set to 'true' if we should run "render" on every page load
  */
  protected def devMode = false
  
  def hasOuter = true
  
  def parentTag = <span/>
  
  private def _handleJson(in: Any): JsCmd = 
  if (jsonHandlerChain.isDefinedAt(in))
  jsonHandlerChain(in)
  else handleJson(in)
  
  
  /**
  * Prepends the handler to the Json Handlers.  Should only be used
  * during instantiation
  *
  * @param h -- the PartialFunction that can handle a JSON request
  */
  def appendJsonHandler(h: PartialFunction[Any, JsCmd]) {
    jsonHandlerChain = h orElse jsonHandlerChain
  }
  
  
  def handleJson(in: Any): JsCmd = Noop
  
  lazy val (jsonCall, jsonInCode) = S.buildJsonFunc(Full(defaultPrefix), _handleJson)
  
  def buildSpan(time: Long, xml: NodeSeq): NodeSeq = Elem(parentTag.prefix, parentTag.label, parentTag.attributes, 
  parentTag.scope, Group(xml)) % (new UnprefixedAttribute("id", Text(uniqueId), Null)) % (new PrefixedAttribute("lift", "when", Text(time.toString), Null))
  
  
  def act = {
    loop {
      react(composeFunction)
    }
  }
  
  def fixedRender: Can[NodeSeq] = Empty
  
  def highPriority : PartialFunction[Any, Unit] = {
    case Never =>
  }
  
  def lowPriority : PartialFunction[Any, Unit] = {
    case Never =>
  }
  
  def mediumPriority : PartialFunction[Any, Unit] = {
    case Never =>
  }  
  
  private def _lowPriority : PartialFunction[Any, Unit] = {
    case s => Log.debug("CometActor "+this+" got unexpected message "+s)
  }
  
  private def _mediumPriority : PartialFunction[Any, Unit] = {
    case Unlisten(seq) => listeners = listeners.filter(_._1 != seq)
    
    case l @ Listen(when, seqId, toDo) =>
    askingWho match {
      case Full(who) => who forward l
      case _ =>
      if (when < lastRenderTime) {
        toDo(AnswerRender(new XmlOrJsCmd(uniqueId, lastRendering, buildSpan _, notices toList), whosAsking openOr this, lastRenderTime, wasLastFullRender))
      } else {
        deltas.filter(_.when > when) match { 
          case Nil => listeners = (seqId, toDo) :: listeners
          
          case all @ (hd :: xs) =>
          toDo( AnswerRender(new XmlOrJsCmd(uniqueId, Empty, Empty, 
          Full(all.reverse.foldLeft(Noop)(_ & _.js)), Empty, buildSpan, false, notices toList), 
          whosAsking openOr this, hd.when, false))
        }
      }
    }
    
    case PerformSetupComet =>
    S.initIfUninitted(theSession) {
      link(ActorWatcher)
      localSetup
      performReRender(true)
    }
    
    case AskRender =>
    askingWho match {
      case Full(who) => who forward AskRender
      case _ => if (!deltas.isEmpty || devMode) performReRender(false); reply(AnswerRender(new XmlOrJsCmd(uniqueId, lastRendering, buildSpan _, notices toList), whosAsking openOr this, lastRenderTime, true))
    }
    
    case ActionMessageSet(msgs, request) =>
    S.init(request, theSession) {
      val ret = msgs.map(_())
      theSession.updateFunctionMap(S.functionMap, uniqueId, lastRenderTime)
      reply(ret)
    }    
    
    case AskQuestion(what, who) =>
    startQuestion(what)
    whosAsking = Full(who)
    
    case AnswerQuestion(what, request) =>
    S.init(request, theSession) {
      askingWho.foreach {
        askingWho =>
        reply("A null message to release the actor from its send and await reply... do not delete this message")
        // askingWho.unlink(self)
        askingWho ! ShutDown
        this.askingWho = Empty
        val aw = answerWith
        answerWith = Empty
        aw.foreach(_(what))
        performReRender(true)
      }
    }
    
    case ReRender(all) => performReRender(all)
    
    case Error(id, node) => notices += (NoticeType.Error, node,  id)

    case Warning(id, node) => notices += (NoticeType.Warning, node,  id)
    
    case Notice(id, node) => notices += (NoticeType.Notice, node,  id)
    
    case ClearNotices => clearNotices
    
    case ShutDown =>
    Log.info("The CometActor "+this+" Received Shutdown")
    theSession.removeCometActor(this)
    localShutdown()
    self.exit("Politely Asked to Exit")
    
    case PartialUpdateMsg(cmd) =>
    val time = CometActor.next
    val delta = JsDelta(time, cmd)
    //val garbageTime = time - 1200000L // remove anything that's more than 20 minutes old
    //deltas = delta :: deltas.filter(_.when < garbageTime)
    deltas = delta :: deltas
    if (!listeners.isEmpty) {
      val rendered = AnswerRender(new XmlOrJsCmd(uniqueId, Empty, Empty, 
      Full(cmd), Empty, buildSpan, false, notices toList), whosAsking openOr this, time, false)
      listeners.foreach(_._2(rendered))
      listeners = Nil
    }
  }
  
  
  /**
  * It's the main method to override, to define what is rendered by the CometActor
  *
  * There are implicit conversions for a bunch of stuff to
  * RenderOut (including NodeSeq).  Thus, if you don't declare the return
  * turn to be something other than RenderOut and return something that's
  * coersable into RenderOut, the compiler "does the right thing"(tm) for you.
  */
  def render: RenderOut
  
  // def compute: Map[String, Any] = Map.empty[String, Any]
  
  def reRender(sendAll: Boolean) {
    this ! ReRender(sendAll)
  }
  
  private def performReRender(sendAll: Boolean) {
    lastRenderTime = CometActor.next
    wasLastFullRender = sendAll & hasOuter
    deltas = Nil
    S.initIfUninitted(theSession) {
      
      lastRendering = render ++ jsonInCode
      theSession.updateFunctionMap(S.functionMap, uniqueId, lastRenderTime)
      
      val rendered: AnswerRender = 
      AnswerRender(new XmlOrJsCmd(uniqueId, lastRendering, buildSpan _, notices toList), 
      this, lastRenderTime, sendAll)
      
      listeners.foreach(_._2(rendered))
      listeners = Nil
      rendered
    }
  }
  
  protected def partialUpdate(cmd: JsCmd) {
    this ! PartialUpdateMsg(cmd)
  }
  
  protected def startQuestion(what: Any) {}
  
  /**
  * This method will be called after the Actor has started.  Do any setup here
  */
  protected def localSetup(): Unit = {}
  
  /**
  * This method will be called as part of the shut-down of the actor.  Release any resources here.
  */
  protected def localShutdown(): Unit = {
    clearNotices
  }
  
  def composeFunction = composeFunction_i
  
  def composeFunction_i = highPriority orElse mediumPriority orElse _mediumPriority orElse lowPriority orElse _lowPriority
  
  def bind(prefix: String, vals: BindParam *): NodeSeq = bind(prefix, defaultXml, vals :_*)
  def bind(vals: BindParam *): NodeSeq = bind(defaultPrefix, vals :_*)
  
  protected def ask(who: CometActor, what: Any)(answerWith: Any => Any) {
    who.start
    theSession.addCometActor(who)
    // who.link(this)
    who ! PerformSetupComet
    askingWho = Full(who)
    this.answerWith = Full(answerWith)
    who ! AskQuestion(what, this)
  }
  
  protected def answer(answer: Any) {
    whosAsking.foreach(_ !? AnswerQuestion(answer, S.request.open_!))
    whosAsking = Empty
    performReRender(false)
  }
  
  implicit def xmlToXmlOrJsCmd(in: NodeSeq): RenderOut = new RenderOut(Full(in), fixedRender, Empty, Empty, false)
  implicit def jsToXmlOrJsCmd(in: JsCmd): RenderOut = new RenderOut(Empty, Empty, Full(in), Empty, false)
  implicit def pairToPair(in: (String, Any)): (String, NodeSeq) = (in._1, Text(in._2 match {case null => "null" case s => s.toString}))
  implicit def nodeSeqToFull(in: NodeSeq): Can[NodeSeq] = Full(in)
  implicit def elemToFull(in: Elem): Can[NodeSeq] = Full(in)
  
  
  
  def error(n: String) {error(Text(n))}
  def error(n: NodeSeq) {notices += (NoticeType.Error, n,  Empty)}
  def error(id:String, n: NodeSeq) {notices += (NoticeType.Error, n,  Full(id))}
  def error(id:String, n: String) {error(id, Text(n))}
  def notice(n: String) {notice(Text(n))}
  def notice(n: NodeSeq) {notices += (NoticeType.Notice, n, Empty)}
  def notice(id:String, n: NodeSeq) {notices += (NoticeType.Notice, n,  Full(id))}
  def notice(id:String, n: String) {notice(id, Text(n))}
  def warning(n: String) {warning(Text(n))}
  def warning(n: NodeSeq) {notices += (NoticeType.Warning, n, Empty)}
  def warning(id:String, n: NodeSeq) {notices += (NoticeType.Warning, n,  Full(id))}
  def warning(id:String, n: String) {warning(id, Text(n))}

  private def clearNotices { notices clear }
  
}

abstract class Delta(val when: Long) {
  def js: JsCmd
}

case class JsDelta(override val when: Long, js: JsCmd) extends Delta(when)

sealed abstract class CometMessage

case class CometActorInitInfo(theSession: LiftSession,name: Can[String],defaultXml: NodeSeq, val attributes: Map[String, String])


class XmlOrJsCmd(val id: String,val xml: Can[NodeSeq],val fixedXhtml: Can[NodeSeq], val javaScript: Can[JsCmd], val destroy: Can[JsCmd],
spanFunc: (Long, NodeSeq) => NodeSeq, ignoreHtmlOnJs: Boolean, notices: List[(NoticeType.Value, NodeSeq, Can[String])]) {
  def this(id: String, ro: RenderOut, spanFunc: (Long, NodeSeq) => NodeSeq, notices: List[(NoticeType.Value, NodeSeq, Can[String])]) =  
    this(id, ro.xhtml,ro.fixedXhtml, ro.script, ro.destroyScript, spanFunc, ro.ignoreHtmlOnJs, notices)
  def toJavaScript(session: LiftSession, displayAll: Boolean): JsCmd = {
    var ret: JsCmd = JsCmds.JsTry(JsCmds.Run("destroy_"+id+"();"), false) &
    ((if (ignoreHtmlOnJs) Empty else xml, javaScript, displayAll) match { 
      case (Full(xml), Full(js), false) => JsCmds.SetHtml(id, xml) & JsCmds.JsTry(js, false)
      case (Full(xml), _, false) => JsCmds.SetHtml(id, xml)
      case (Full(xml), Full(js), true) => JsCmds.SetHtml(id+"_outer", (spanFunc(0, xml) ++
        fixedXhtml.openOr(Text("")))) & JsCmds.JsTry(js, false)
      case (Full(xml), _, true) => JsCmds.SetHtml(id+"_outer", (spanFunc(0, xml) ++
        fixedXhtml.openOr(Text(""))))
      case (_, Full(js), _) => js
      case _ => JsCmds.Noop
    }) & JsCmds.JsTry(JsCmds.Run("destroy_"+id+" = function() {"+(destroy.openOr(JsCmds.Noop).toJsCmd)+"};"), false)
    S.initIfUninitted(session) {
       S.messagesFromList(notices toList)
       ret = ret & S.noticesToJsCmd
    }
    ret
  }
  
  def inSpan: NodeSeq = xml.openOr(Text(""))++javaScript.map(s => Script(s)).openOr(Text("")) 
  
  def outSpan: NodeSeq = Script(Run("var destroy_"+id+" = function() {"+(destroy.openOr(JsCmds.Noop).toJsCmd)+"}")) ++
  fixedXhtml.openOr(Text(""))
}

case class PartialUpdateMsg(cmd: JsCmd) extends CometMessage
case object AskRender extends CometMessage
case class AnswerRender(response: XmlOrJsCmd, who: CometActor, when: Long, displayAll: Boolean) extends CometMessage
case object PerformSetupComet extends CometMessage
case class AskQuestion(what: Any, who: CometActor) extends CometMessage
case class AnswerQuestion(what: Any, request: RequestState) extends CometMessage
case class Listen(when: Long, uniqueId: ListenerId, action: AnswerRender => Unit) extends CometMessage
case class Unlisten(uniqueId: ListenerId) extends CometMessage
case class ActionMessageSet(msg: List[() => Any], request: RequestState) extends CometMessage
case class ReRender(doAll: Boolean) extends CometMessage
case class ListenerId(id: Long)
case class Error(id: Can[String], msg: NodeSeq) extends CometMessage 
case class Warning(id: Can[String], msg: NodeSeq) extends CometMessage 
case class Notice(id: Can[String], msg: NodeSeq) extends CometMessage 
case object ClearNotices extends CometMessage

object Error {
  def apply(node: NodeSeq): Error = Error(Empty, node)
  def apply(node: String): Error = Error(Empty, Text(node))
  def apply(id: String, node: String): Error = Error(Full(id), Text(node))
  def apply(id: String, node: NodeSeq): Error = Error(Full(id), node)
}
object Warning {
  def apply(node: NodeSeq): Warning = Warning(Empty, node)
  def apply(node: String): Warning = Warning(Empty, Text(node))
  def apply(id: String, node: String): Warning = Warning(Full(id), Text(node))
  def apply(id: String, node: NodeSeq): Warning = Warning(Full(id), node)
}
object Notice {
  def apply(node: NodeSeq): Notice = Notice(Empty, node)
  def apply(node: String): Notice = Notice(Empty, Text(node))
  def apply(id: String, node: String): Notice = Notice(Full(id), Text(node))
  def apply(id: String, node: NodeSeq): Notice = Notice(Full(id), node)
}

/**
* @param xhtml is the "normal" render body
* @param fixedXhtml is the "fixed" part of the body.  This is ignored unless reRender(true)
* @param script is the script to be executed on render.  This is where you want to put your script
* @param destroyScript is executed when the comet widget is redrawn ( e.g., if you register drag or mouse-over or some events, you unregister them here so the page doesn't leak resources.)
* @param ignoreHtmlOnJs -- if the reason for sending the render is a Comet update, ignore the xhtml part and just run the JS commands.  This is useful in IE when you need to redraw the stuff inside <table><tr><td>... just doing innerHtml on <tr> is broken in IE 
*/
@serializable
case class RenderOut(xhtml: Can[NodeSeq], fixedXhtml: Can[NodeSeq], script: Can[JsCmd], destroyScript: Can[JsCmd], ignoreHtmlOnJs: Boolean) {
  def this(xhtml: NodeSeq) = this(Full(xhtml), Empty, Empty, Empty, false)
  def this(xhtml: NodeSeq, js: JsCmd) = this(Full(xhtml), Empty, Full(js), Empty, false)
  def this(xhtml: NodeSeq, js: JsCmd, destroy: JsCmd) = this(Full(xhtml), Empty, Full(js), Full(destroy), false) 
  def ++(cmd: JsCmd) = 
  RenderOut(xhtml, fixedXhtml, script.map(_ & cmd) or Full(cmd),
  destroyScript, ignoreHtmlOnJs)
}

@serializable
private[http] object Never
