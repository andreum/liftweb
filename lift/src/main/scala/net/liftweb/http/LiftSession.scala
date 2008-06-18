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
package net.liftweb.http

import scala.actors.Actor
import scala.actors.Actor._
import javax.servlet.http.{HttpSessionBindingListener, HttpSessionBindingEvent, HttpSession}
import scala.collection.mutable.{HashMap, ArrayBuffer, ListBuffer}
import scala.xml.{NodeSeq, Unparsed, Text}
import net.liftweb.mapper.DB
import net.liftweb.util._
import net.liftweb.util.Helpers._
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import scala.xml.{Node, NodeSeq, Elem, MetaData, Null, UnprefixedAttribute, PrefixedAttribute, XML, Comment, Group}
import java.io.InputStream
import javax.servlet.http.{HttpSessionActivationListener, HttpSessionEvent, HttpServletRequest}
import scala.xml.transform._
import java.util.concurrent.TimeUnit

object LiftSession {
  
  def createSession(session: HttpSession,  contextPath: String) = (new LiftSession(contextPath, session.getId, session))
  
  var creator = createSession _
  
  def apply(session: HttpSession, contextPath: String) =
  creator(session, contextPath)
  
  
  var onSessionActivate: List[LiftSession => Unit] = Nil
  var onSessionPassivate: List[LiftSession => Unit] = Nil
  var onSetupSession: List[LiftSession => Unit] = Nil
  var onShutdownSession: List[LiftSession => Unit] = Nil
  var onBeginServicing: List[(LiftSession, RequestState) => Unit] = Nil
  var onEndServicing: List[(LiftSession, RequestState, Can[ResponseIt]) => Unit] = Nil
}


private[http] case class AddSession(session: LiftSession)
private[http] case class RemoveSession(sessionId: String)
case class SessionWatcherInfo(sessions: Map[String, LiftSession])

@serializable
case class SessionToServletBridge(uniqueId: String) extends HttpSessionBindingListener with HttpSessionActivationListener {
  def sessionDidActivate(se: HttpSessionEvent) = {
    SessionMaster.getSession(uniqueId).foreach(ls => 
    LiftSession.onSessionActivate.foreach(_(ls)))
  }
  
  def sessionWillPassivate(se: HttpSessionEvent) = {
    SessionMaster.getSession(uniqueId).foreach(ls => 
    LiftSession.onSessionPassivate.foreach(_(ls)))
  }
  
  def valueBound(event: HttpSessionBindingEvent) {

  }
  
  
  /**
  * When the session is unbound the the HTTP session, stop us
  */
  def valueUnbound(event: HttpSessionBindingEvent) {
    SessionMaster ! RemoveSession(uniqueId)
    
  }
  
}

/**
* Manages LiftSessions because the servlet container is less than optimal at
* timing sessions out.
*/
object SessionMaster extends Actor {
  private case class LookupSession(uniqueId: String, when: Long)
  
  private var sessions: Map[String, LiftSession] = Map.empty
  private object CheckAndPurge
  
  def getSession(id: String): Can[LiftSession] = {
    val when = CometActor.next
    this ! LookupSession(id, when)
    def looper: Can[LiftSession] = 
    self.receiveWithin(100) {
      case (respWhen, _) if respWhen != when => looper
      case (respWhen, Full(ls: LiftSession)) => Full(ls)
      case _ => Empty
    }
    
    looper
  }

  /**
  * Put an Actor in this list and the Actor will receive a message
  * every 10 seconds with the current list of sessions:
  * SessionWatcherInfo
  */
  var sessionWatchers: List[Actor] = Nil
  
  def getSession(httpSession: HttpSession): Can[LiftSession] =
  getSession(httpSession.getId())
  
  def getSession(req: HttpServletRequest): Can[LiftSession] =
  getSession(req.getSession)
  
  def addSession(liftSession: LiftSession) {
    this ! AddSession(liftSession)
  }
  
  private val LiftMagicID = "$lift_magic_session_thingy$"
  
  def act = {
    doPing()
    link(ActorWatcher)
    loop {
      react {
        case AddSession(session) =>
        try {
          if (!sessions.contains(session.uniqueId)) {
            sessions = sessions + (session.uniqueId -> session)
            session.startSession()
            val b = SessionToServletBridge(session.uniqueId)
            session.httpSession.setAttribute(LiftMagicID, b)
          }
        } catch {
          case e => Log.error("Problem adding session", e)
        }
        
        case RemoveSession(sessionId) =>
        sessions.get(sessionId).foreach{s =>
          try {
            s.doShutDown
            try {
              s.httpSession.removeAttribute(LiftMagicID)
            } catch {
              case e => // ignore... sometimes you can't do this and it's okay
            }
          } catch {
            case e => Log.error("Failure in remove session", e)
            
          } finally {
            sessions = sessions - sessionId
          }
        }
        
        case LookupSession(id, when) =>
        reply( (when, Can(sessions.get(id))) )
        
        case CheckAndPurge =>
        val now = millis
        for ((id, session) <- sessions.elements) {
          if (now - session.lastServiceTime > session.inactivityLength) {
            Log.info(" Session "+id+" expired")
            this ! RemoveSession(id)
          }
        }
        sessionWatchers.foreach(_ ! SessionWatcherInfo(sessions))
        doPing()
      }
    }
  }
  
  this.start
  
  private def doPing() {
    try {
      ActorPing schedule(this, CheckAndPurge, 10 seconds)
    } catch {
      case e => Log.error("Couldn't start SessionMaster ping", e)
    }
  }
  
  
}

@serializable
class LiftSession(val contextPath: String, val uniqueId: String, val httpSession: HttpSession) { 
  import TemplateFinder._
  
  private var running_? = false
  
  private var messageCallback: HashMap[String, S.AFuncHolder] = new HashMap
  
  private[http] var notices: Seq[(NoticeType.Value, NodeSeq, Can[String])] = Nil
  
  private var asyncComponents = new HashMap[(Can[String], Can[String]), CometActor]()
  
  private var asyncById = new HashMap[String, CometActor]()
  
  private var myVariables: Map[String, Any] = Map.empty

  @volatile
  private[http] var lastServiceTime = millis
  
  @volatile
  private[http] var inactivityLength = 180000L
  
  private [http] var highLevelSessionDispatcher = new HashMap[String, LiftRules.DispatchPf]()
  private [http] var sessionTemplater = new HashMap[String, LiftRules.TemplatePf]()
  private [http] var sessionRewriter = new HashMap[String, LiftRules.RewritePf]()
  
  private[http] def startSession(): Unit = {
    running_? = true
    inactivityLength = httpSession.getMaxInactiveInterval.toLong * 1000L
    lastServiceTime = millis
    LiftSession.onSetupSession.foreach(_(this))
  }
  
  private var cometList: List[Actor] = Nil
  
  private[http] def breakOutComet(): Unit = synchronized {
    cometList.foreach(_ ! BreakOut)
  }
  
  private[http] def enterComet(what: Actor): Unit = synchronized {
    cometList = what :: cometList
  }
  
  private[http] def exitComet(what: Actor): Unit = synchronized {
    cometList = cometList.remove(_ eq what)
  }
  
  private case class RunnerHolder(name: String, func: S.AFuncHolder, owner: Can[String])
  
  def runParams(state: RequestState): List[Any] = {
    val toRun = synchronized {
      // get all the commands, sorted by owner,
      (state.uploadedFiles.map(_.name) ::: state.paramNames).filter(n => messageCallback.contains(n)).
      map{n => val mcb = messageCallback(n);  RunnerHolder(n, mcb, mcb.owner)}.
      sort{
        case ( RunnerHolder(_, _, Full(a)), RunnerHolder(_, _, Full(b))) if a < b => true
        case (RunnerHolder(_, _, Full(a)), RunnerHolder(_, _, Full(b))) if a > b => false
        case (RunnerHolder(an, _, Full(a)), RunnerHolder(bn, _, Full(b))) if a == b => an < bn
        case (RunnerHolder(_,_, Full(_)), _) => false
        case (_, RunnerHolder(_, _, Full(_))) => true
        case (RunnerHolder(a, _, _), RunnerHolder(b, _, _)) => a < b
      }
    }
    
    def buildFunc(i: RunnerHolder): () => Any = i.func match {
      case bfh: S.BinFuncHolder => () => state.uploadedFiles.filter(_.name == i.name).map(v => bfh(v))
      case normal => () => normal(state.params.getOrElse(i.name, state.uploadedFiles.filter(_.name == i.name).map(_.fileName)))
    }
    
    val ret = toRun.map(_.owner).removeDuplicates.flatMap{w =>
      val f = toRun.filter(_.owner == w);
      w match {
        // if it's going to a CometActor, batch up the commands
        case Full(id) =>
        
        asyncById.get(id).toList.flatMap(a => a !? ActionMessageSet(f.map(i => buildFunc(i)), state) match {case Some(li: List[Any]) => li case li: List[Any] => li case other => Nil})
        case _ => f.map(i => buildFunc(i).apply())
      }
    }
    
    ret
  }
  
  private[http] def updateFunctionMap(funcs: Map[String, S.AFuncHolder]): Unit = synchronized {
    funcs.foreach(mi => messageCallback(mi._1) = mi._2)
  }
  
  def updateFunctionMap(funcs: Map[String, S.AFuncHolder], uniqueId: String, when: Long): Unit = synchronized {
    funcs.foreach{case (name, func) => messageCallback(name) = func.duplicate(uniqueId)}
  }
  
  /**
  * Called just before the session exits.  If there's clean-up work, override this method
  */
  def cleanUpSession() {
    
  }
  
  private[http] def doShutDown() {
    try {
      if (running_?) this.shutDown()
    } finally {
      Actor.clearSelf
      DB.clearThread
    }
  }
  
  private def shutDown() = synchronized {
    SessionMaster ! RemoveSession(this.uniqueId)
    
    // Log.debug("Shutting down session")
    running_? = false
    asyncComponents.foreach{case (_, comp) => comp ! ShutDown}
    cleanUpSession()
    LiftSession.onShutdownSession.foreach(_(this))
  }
  
  private[http] def processRequest(request: RequestState): Can[ResponseIt] = {
    S.init(request, notices, this) {
      LiftSession.onBeginServicing.foreach(_(this, request))
      val ret = try {
        val sessionDispatch = S.highLevelSessionDispatcher
        val toMatch = RequestMatcher(request, Full(this))
        if (sessionDispatch.isDefinedAt(toMatch)) {
          runParams(request)
          try {
            sessionDispatch(toMatch)(request) match {
              case Full(r) => Full(checkRedirect(r))
              case _ => LiftRules.notFoundOrIgnore(request, Full(this))
            }
          } finally {
            notices = S.getNotices
          }
        } else {
          runParams(request)
          
          def idAndWhen(in: Node): Can[(String, String)] = 
          ((in \ "@id").toList, in.attributes.filter{case p: PrefixedAttribute => (p.pre == "lift" && p.key == "when") case _ => false}.toList) match {
            case (x :: _, y :: _) => Full((x.text,y.value.text))
            case _ => Empty
          }
          
          // Process but make sure we're okay, sitemap wise
          val response: Can[ResponseIt] = request.testLocation match {
            case (true, _) => (findVisibleTemplate(request.path, request).map(xml => processSurroundAndInclude(request.uri+" -> "+request.path, xml)) match {
              case Full(rawXml: NodeSeq) => {
                val xml = HeadHelper.mergeToHtmlHead(rawXml)
                val realXml = allElems(xml, !_.attributes.filter{case p: PrefixedAttribute => (p.pre == "lift" && p.key == "when") case _ => false}.toList.isEmpty) match {
                  case Nil => xml
                  case xs => val comets: List[(String, String)] = xs.flatMap(x => idAndWhen(x))
                  val cometVar = "var lift_toWatch = "+comets.map{case (a,b) => ""+a+": '"+b+"'"}.mkString("{", " , ", "}")+";"
                  val hasJQuery: Boolean = !(xml \\ "script").toList.filter(s => (s \ "@src").toList.map(_.text).mkString("").toLowerCase.indexOf("jquery") >= 0).isEmpty
                  
                  val xform = new RuleTransformer(new AddScriptToBody(cometVar) :: (if (!hasJQuery) List(new AddScriptTag) else Nil) :_*)
                  xform.transform(xml)
                }
                
                this.synchronized {
                  S.functionMap.foreach(mi => messageCallback(mi._1) = mi._2)
                }
                notices = Nil // S.getNotices
                Full(LiftRules.convertResponse((realXml,
                S.getHeaders(LiftRules.defaultHeaders((realXml, request))),
                S.responseCookies,
                request)))
              }
              case _ => if (LiftRules.passNotFoundToChain) Empty else Full(request.createNotFound)
            })
            case (false, Empty) => if (LiftRules.passNotFoundToChain) Empty else Full(request.createNotFound)
            case (false, msg) => msg
          }
          
          // Before returning the response check for redirect and set the appropriate state.
          response.map(checkRedirect) 
        }
      } catch {
        case ite: java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[ResponseShortcutException]) =>
        Full(handleRedirect(ite.getCause.asInstanceOf[ResponseShortcutException], request))
        
        case rd: net.liftweb.http.ResponseShortcutException => Full(handleRedirect(rd, request))
        
        case e  => Full(LiftRules.logAndReturnExceptionToBrowser(request, e))
        
      }
      
      LiftSession.onEndServicing.foreach(_(this, request, ret))
      ret
    }
  }
  
  private def handleRedirect(re: ResponseShortcutException, request: RequestState): ResponseIt = {
    if (re.doNotices) notices = S.getNotices
    
    re.response 
  }
  
  /**
  * Set a session-local variable to a value
  *
  * @param name -- the name of the variable
  * @param value -- the value of the variable
  */
  def set[T](name: String, value: T): Unit = synchronized {
    myVariables = myVariables + (name -> value)
  }
  
  /**
  * Gets the named variable if it exists
  *
  * @param name -- the name of the session-local variable to get
  * 
  * @return Full(value) if found, Empty otherwise
  */
  def get[T](name: String): Can[T] = synchronized {
    myVariables.get(name) match {
      case Some(v: T) => Full(v)
      case _ => Empty
    }
  }
  
  /**
  * Gets the named variable if it exists
  *
  * @param name -- the name of the session-local variable to get
  * @param clz -- the class of the return type
  * 
  * @return Full(value) if found, Empty otherwise
  */
  def get[T](name: String, clz: Class[T]): Can[T] = synchronized {
    myVariables.get(name) match {
      case Some(v) => Full(v) isA clz
      case _ => Empty
    }
  }
  
  /**
  * Unset the named variable
  *
  * @param name the variable to unset
  */
  def unset(name: String): Unit = synchronized {
    myVariables -= name
  }
  
  private[http] def attachRedirectFunc(uri: String, f : Can[() => Unit]) = {
    f map { fnc => 
      val func: String = LiftSession.this.synchronized {
        val funcName = "fn"+randomString(20)
        messageCallback(funcName) = S.NFuncHolder(() => {
          try {
            fnc()
          } finally {
            LiftSession.this.synchronized {
              messageCallback -= funcName
            }
          }
        })
        funcName
      }
      val sep = uri contains("?") match {case true => "&" case _ => "?"}
      uri + sep + func +"=_"
    } openOr uri
    
  }
  
  private[http] def checkRedirect(resp: ResponseIt): ResponseIt = resp match {
    case RedirectWithState(uri, state, cookies @ _*) => 
    state.msgs.foreach(m => S.message(m._1, m._2))
    notices = S.getNotices
    RedirectResponse(attachRedirectFunc(uri, state.func), cookies:_*)
    case _ => resp
  }
  
  
  private def allElems(in: NodeSeq, f: Elem => Boolean): List[Elem] = {
    val lb = new ListBuffer[Elem]
    
    def appendAll(in: NodeSeq, lb: ListBuffer[Elem]) {
      in.foreach{
        case Group(ns) => appendAll(ns, lb)
        case e: Elem if f(e) => lb += e; appendAll(e.child, lb)
        case e: Elem => appendAll(e.child, lb)
        case _ =>
      }
    }
    appendAll(in, lb)
    
    lb.toList
  }
  
  private def findVisibleTemplate(path: ParsePath, session: RequestState) : Can[NodeSeq] = {
    val toMatch = RequestMatcher(session, Full(this))
    val templ = LiftRules.templateTable(session.request)
    (if (templ.isDefinedAt(toMatch)) templ(toMatch)() else Empty) or {
      val tpath = path.path
      val splits = tpath.toList.filter {a => !a.startsWith("_") && !a.startsWith(".") && a.toLowerCase.indexOf("-hidden") == -1} match {
        case s @ _ if (!s.isEmpty) => s
        case _ => List("index")
      }
      findAnyTemplate(splits)
    }
  }
  
  private def findTemplate(name: String) : Can[NodeSeq] = {
    val splits = (if (name.startsWith("/")) name else "/"+name).split("/").toList.drop(1) match {
      case Nil => List("index")
      case s => s
    }
    
    findAnyTemplate(splits) or findAnyTemplate("templates-hidden" :: splits)
  }
  
  def couldBeHtml(in : List[(String, String)]) : Boolean = {
    in match {
      case null | Nil => true
      case _ => in.ciGet("Content-Type").map(_.toLowerCase.contains("html")) openOr true
    }
  }
  
  
  private def findAndEmbed(templateName: Can[Seq[Node]], kids : NodeSeq) : NodeSeq = {
    templateName match {
      case Full(tn) => {
        findTemplate(tn.text) match {
          case Full(s) => processSurroundAndInclude(tn.text, s)
          case _ => Comment("FIX"+"ME Unable to find template named "+tn.text) ++ kids
        }
      }
      case _ => Comment("FIX"+"ME No named specified for embedding") ++ kids
    }
  }
  
  private def findSnippetClass[C <: AnyRef](name: String, bound: Class[C]): Can[Class[C]] = {
    if (name == null) Empty
    else findClass(name, LiftRules.buildPackage("snippet") ::: ("lift.app.snippet" :: "net.liftweb.builtin.snippet" :: Nil), bound)
  }
  
  private def findAttributeSnippet(name: String, rest: MetaData): MetaData = {
    val (cls, method) = splitColonPair(name, null, "render")
    findSnippetClass(cls, classOf[AnyRef]).flatMap(clz => instantiate(clz).flatMap(inst => tryo(invokeMethod(clz, inst, method) match {
      case Full(md: MetaData) => Full(md.copy(rest))
      case _ => Empty
    }).flatMap(s => s))).openOr(rest)
  }
  
  
  private def processAttributes(in: MetaData) : MetaData = {
    in match {
      case Null => Null
      case mine: PrefixedAttribute if (mine.pre == "lift") => {
        mine.key match {
          case "snippet" => findAttributeSnippet(mine.value.text, processAttributes(in.next))
          case _ => mine.copy(processAttributes(in.next))
        }
      }
      case notMine => notMine.copy(processAttributes(in.next))
    }
  }
  
  
  private def findSnippetInstance(cls: String): Can[AnyRef] =
  S.snippetForClass(cls) or
  (findSnippetClass(cls, classOf[AnyRef]).flatMap(c => instantiate(c)) match {
    case Full(inst: StatefulSnippet) =>
    inst.snippetName = cls; S.setSnippetForClass(cls, inst); Full(inst)
    case Full(ret) => Full(ret)
    case _ => Empty
  })
  
  
  private def processSnippet(page: String, snippetName: Can[String], attrs: MetaData, kids: NodeSeq): NodeSeq = {
    val isForm = !attrs.get("form").toList.isEmpty
    val ret = snippetName.map(snippet =>
    S.locateSnippet(snippet).map(_(kids)) openOr {
      val (cls, method) = splitColonPair(snippet, null, "render")
      findSnippetInstance(cls) match {
        
        case Full(inst: StatefulSnippet) =>
        if (inst.dispatch.isDefinedAt(method))
        (if (isForm) SHtml.hidden(ignore => inst.registerThisSnippet) else Text("")) ++ 
        inst.dispatch(method)(kids)
        else {LiftRules.snippetFailedFunc.foreach(_(LiftRules.SnippetFailure(page, snippetName, 
        LiftRules.SnippetFailures.StatefulDispatchNotMatched))); kids}
        
        case Full(inst) => {
          val ar: Array[Object] = List(Group(kids)).toArray
          ((invokeMethod(inst.getClass, inst, method, ar)) or invokeMethod(inst.getClass, inst, method)) match {
            case Full(md: NodeSeq) => md
            case it => LiftRules.snippetFailedFunc.foreach(_(LiftRules.SnippetFailure(page, snippetName, 
            LiftRules.SnippetFailures.MethodNotFound))); kids
          }
        }
        case _ => LiftRules.snippetFailedFunc.foreach(_(LiftRules.SnippetFailure(page, snippetName, 
        LiftRules.SnippetFailures.ClassNotFound))); kids
      }
    }).openOr{
      LiftRules.snippetFailedFunc.foreach(_(LiftRules.SnippetFailure(page, snippetName, 
      LiftRules.SnippetFailures.NoNameSpecified)))
      Comment("FIX"+"ME -- no type defined for snippet")
      kids
    }
    
    def checkMultiPart(in: MetaData): MetaData = in.filter(_.key == "multipart").toList match {
      case Nil => Null
      case x => new UnprefixedAttribute("enctype", Text("multipart/form-data"), Null)
    }
    
    attrs.get("form").map(ft => <form action={S.uri} method={ft.text}>{ret}</form> % checkMultiPart(attrs)) getOrElse ret
  }
  
  
  def fixHtml(in: NodeSeq): NodeSeq = RequestState.fixHtml(contextPath, in)
  
  
  /**
  * The partial function that defines how lift tags are processed for this session.  Initially composed
  * of LiftRules.liftTagProcessing orElse the default lift tag processing.  If you need to change the
  * way a particular session handles lift tags, alter this partial function.
  */
  var liftTagProcessing: LiftRules.LiftTagPF = _
  
  /**
  * The basic partial function that does lift tag processing
  */
  private def _defaultLiftTagProcessing: LiftRules.LiftTagPF = {
    case ("snippet", elm, metaData, kids, page) =>
    metaData.get("type") match {
      case Some(tn) => liftTagProcessing((tn.text, elm, metaData, kids, page))
      case _ => processSnippet(page, Empty , elm.attributes, elm.child)
    }
    case ("surround", elm, _, _, page) => processSurroundElement(page, elm)
    case ("embed", _, metaData, kids, page) => findAndEmbed(Can(metaData.get("what")), kids)
    case ("ignore", _, _, _, _) => Text("")
    case ("comet", _, metaData, kids, _) => executeComet(Can(metaData.get("type").map(_.text.trim)), Can(metaData.get("name").map(_.text.trim)), kids, metaData)
    case ("children", _, _, kids, _) => kids
    case ("a", elm, metaData, kids, _) => Elem(null, "a", addAjaxHREF(metaData), elm.scope, kids :_*)
    case ("form", elm, metaData, kids, _) => Elem(null, "form", addAjaxForm(metaData), elm.scope, kids : _*)
    case ("loc", elm, metaData, kids, _) => metaData.get("locid") match {case Some(id) => S.loc(id.text, kids) case _ => S.loc(kids.text, kids)}
    case (snippetInfo, elm, metaData, kids, page) => processSnippet(page, Full(snippetInfo) , metaData, kids)
  }
  
  liftTagProcessing = LiftRules.liftTagProcessing orElse _defaultLiftTagProcessing
  
  def processSurroundAndInclude(page: String, in: NodeSeq): NodeSeq = {
    in.flatMap{
      v =>
      v match {
        case Group(nodes) => Group(processSurroundAndInclude(page, nodes))
        case elm: Elem if elm.prefix == "lift" || elm.prefix == "l" => S.setVars(elm.attributes)(processSurroundAndInclude(page, liftTagProcessing(elm.label, elm, elm.attributes, elm.child, page)))
        case elm: Elem => Elem(v.prefix, v.label, processAttributes(v.attributes), v.scope, processSurroundAndInclude(page, v.child) : _*)
        case _ => v
      }
    }
  }
  
  private def executeComet(theType: Can[String], name: Can[String], kids: NodeSeq, attr: MetaData): NodeSeq = {
    try {
      findComet(theType, name, kids, Map.empty ++
      attr.flatMap{
        case u: UnprefixedAttribute => List((u.key, u.value.text))
        case u: PrefixedAttribute => List((u.pre+":"+u.key, u.value.text))
      case _ => Nil}.toList).
      map(c =>
      (c !? (26600, AskRender)) match {
        case Some(AnswerRender(response, _, when, _)) if c.hasOuter =>
        <span id={c.uniqueId+"_outer"}>{c.buildSpan(when, response.inSpan)}{response.outSpan}</span>
        
        case Some(AnswerRender(response, _, when, _)) =>
        c.buildSpan(when, response.inSpan)
        
        case _ => <span id={c.uniqueId} lift:when="0">{Comment("FIX"+"ME comet type "+theType+" name "+name+" timeout") ++ kids}</span>
      }) openOr Comment("FIX"+"ME - comet type: "+theType+" name: "+name+" Not Found ") ++ kids
    } catch {
      case e => Log.error("Failed to find a comet actor", e); kids
    }
  }
  
  def findComet(theType: String): List[CometActor] = synchronized {
    asyncComponents.elements.filter{case ((Full(name), _), _) => name == theType case _ => false}.toList.map{case (_, value) => value}
  }
  
  private def findComet(theType: Can[String], name: Can[String], defaultXml: NodeSeq, attributes: Map[String, String]): Can[CometActor] = {
    val what = (theType, name)
    Can(asyncComponents.get(what)).or( {
      theType.flatMap{
        tpe =>
        val ret = findCometByType(tpe, name, defaultXml, attributes)
        ret.foreach(r =>
        synchronized {
          asyncComponents(what) = r
          asyncById(r.uniqueId) = r
        })
        ret
      }
    })
  }
  
  def getAsyncComponent(id: String): Can[CometActor] = synchronized(asyncById.get(id))
  
  def addCometActor(act: CometActor): Unit = synchronized {
    asyncById(act.uniqueId) = act
  }
  
  def removeCometActor(act: CometActor): Unit = synchronized {
    asyncById -= act.uniqueId
    messageCallback -= act.jsonCall.funcId
    
    // FIXME remove all the stuff from the function table related to this item
    
  }
  
  private def findCometByType(contType: String, name: Can[String], defaultXml: NodeSeq, attributes: Map[String, String]): Can[CometActor] = {
    findClass(contType, LiftRules.buildPackage("comet") ::: ("lift.app.comet" :: Nil), classOf[CometActor]).flatMap{
      cls =>
      tryo((e: Throwable) => e match {case e: java.lang.NoSuchMethodException => ()
      case e => Log.info("Comet find by type Failed to instantiate "+cls.getName, e)}) {
        val constr = cls.getConstructor(Array(classOf[CometActorInitInfo]))
        val ret = constr.newInstance(Array(CometActorInitInfo(this, name, defaultXml, attributes))).asInstanceOf[CometActor];
        ret.start
        // ret.link(this)
        ret ! PerformSetupComet
        ret.asInstanceOf[CometActor]
      }  or tryo((e: Throwable) => Log.info("Comet find by type Failed to instantiate "+cls.getName, e)) {
        val constr = cls.getConstructor(Array(this.getClass , classOf[Can[String]], classOf[NodeSeq], classOf[Map[String, String]]))
        val ret = constr.newInstance(Array(this, name, defaultXml, attributes)).asInstanceOf[CometActor];
        ret.start
        // ret.link(this)
        ret ! PerformSetupComet
        ret.asInstanceOf[CometActor]
      }
    }
  }
  
  
  
  private def addAjaxHREF(attr: MetaData): MetaData = {
    val ajax = "jQuery.ajax( {url: '"+S.encodeURL(contextPath+"/"+LiftRules.ajaxPath)+"', timeout: 10000, cache: false, data: '"+attr("key")+"=true', dataType: 'script'});"
    new UnprefixedAttribute("onclick", Text(ajax), new UnprefixedAttribute("href", Text("javascript://"), attr.filter(a => a.key != "onclick" && a.key != "href")))
  }
  
  private def addAjaxForm(attr: MetaData): MetaData = {
    val id = "F"+randomString(15)
    val pre = attr.filter(_.key == "onsubmit").toList match {
      case Nil => ""
      case x :: xs => x.value.text +";"
    }
    val ajax = "jQuery.ajax( {url: '"+S.encodeURL(contextPath+"/"+LiftRules.ajaxPath)+"', timeout: 10000, cache: false, data: jQuery('#"+id+"').serialize(), dataType: 'script', type: 'POST'}); "+pre+" return false;"
    new UnprefixedAttribute("id", Text(id), new UnprefixedAttribute("action", Text("#"), new UnprefixedAttribute("onsubmit", Text(ajax), attr.filter(a => a.key != "id" && a.key != "onsubmit" && a.key != "action"))))
  }
  
  
  /** Split seq into two seqs: first matches p, second matches !p */
  private def filter2[A](c: Seq[A])(p: A => Boolean): (Seq[A], Seq[A]) = {
    val bufs = (new ArrayBuffer[A], new ArrayBuffer[A])
    val i = c.elements
    while (i.hasNext) {
      val x = i.next
      if (p(x)) bufs._1 += x
      else bufs._2 += x
    }
    bufs
  }
  
  private def processSurroundElement(page: String, in: Elem): NodeSeq = {
    val attr = in.attributes
    val kids = in.child
    // case Elem("lift", "surround", attr @ _, _, kids @ _*) =>
    
    val (otherKids, paramElements) = filter2(kids) {
      case Elem("lift", "with-param", _, _, _) => false
      case _ => true
    }
    
    val params = paramElements.flatMap {
      case Elem("lift", "with-param", attr @ _, _, kids @ _*) =>
      val valueOption: Option[Seq[Node]] = attr.get("name")
      val option: Option[(String, NodeSeq)] = valueOption.map((v: Seq[Node]) => (v.text, processSurroundAndInclude(page, kids)))
      option
    }
    
    val mainParam = (attr.get("at").map(_.text: String).getOrElse("main"),
    processSurroundAndInclude(page, otherKids))
    val paramsMap = collection.immutable.Map(params: _*) + mainParam
    findAndMerge(attr.get("with"), paramsMap)
  }
  
  private def findAndMerge(templateName: Can[Seq[Node]], atWhat: Map[String, NodeSeq]): NodeSeq = {
    val name = templateName.map(s => if (s.text.startsWith("/")) s.text else "/"+ s.text).openOr("/templates-hidden/default")
    
    findTemplate(name).map(s => processBind(s, atWhat)).
    openOr(atWhat.values.flatMap(_.elements).toList)
  }
  
  class AddScriptTag extends RewriteRule {
    override def transform(n: Node) = n match {
      case Elem(null, "head", attr @ _, scope @ _, kids @ _*) =>
      Elem(null, "head", attr,  scope, (kids ++ <script src="/classpath/jquery.js" type="text/javascript"/>) :_*)
      case _ => n
    }
  }
  
  /**
  * Add comet script juste before the &lt;/body> tag.
  * Instance of the class can't be reused, because it does the transformation once,
  * to avoid several transformation when there is several body tag into html.
  */
  class AddScriptToBody(val cometVar: String) extends RewriteRule {
    var done = false
    override def transform(n: Node) = n match {
      case Elem(null, "body", attr @ _, scope @ _, kids @ _*) if !done =>
      done = true
      Elem(null, "body", attr,  scope, (kids ++ <span id="lift_bind"/><script>
      // { Unparsed("""<![CDATA[
      """+cometVar+"""
      function lift_handlerSuccessFunc() {setTimeout("lift_cometEntry();",100);}
      function lift_handlerFailureFunc() {setTimeout("lift_cometEntry();",10000);}
      function lift_cometEntry() {jQuery.ajax( {url: '"""+S.encodeURL(contextPath+"/"+LiftRules.cometPath)+"""', cache: false, success: lift_handlerSuccessFunc, timeout: 140000, data: lift_toWatch, dataType: 'script', error: lift_handlerFailureFunc} );}
      jQuery(document).ready(function(){lift_handlerSuccessFunc();});
      // ]]>
      """)}</script>) :_*)
      case _ => n
    }
  }
}

abstract class SessionMessage


/**
* The response from a page saying that it's been rendered
*/
case object ShutDown

case class AnswerHolder(what: ResponseIt)

/**
* If a class is to be used as a lift view (rendering from code rather than a static template)
* and the method names are to be used as "actions", the view must be marked as "InsecureLiftView"
* because there exists the ability to execute arbitrary methods based on wire content
*/
trait InsecureLiftView

/**
*  The preferred way to do lift views... implement a partial function that dispatches
* the incoming request to an appropriate method
*/
trait LiftView {
  implicit def nsToCns(in: NodeSeq): Can[NodeSeq] = Can.legacyNullTest(in)
  def dispatch_& : PartialFunction[String, () => Can[NodeSeq]]
}

object TemplateFinder {
  private val suffixes = List("", "html", "xhtml", "htm")
  
  /**
  * Given a list of paths (e.g. List("foo", "index")),
  * find the template.
  * @param places - the path to look in
  *
  * @return the template if it can be found
  */
  def findAnyTemplate(places : List[String]): Can[NodeSeq] = {
    val pls = places.mkString("/","/", "")
    val toTry = for (s <- suffixes; p <- locales) yield pls + p + (if (s.length > 0) "." + s else "")
    
    first(toTry)(v => LiftRules.finder(v).flatMap(fc => PCDataXmlParser(fc))) or lookForClasses(places)
  }
  
  private def locales: List[String] = {
    val locale = S.locale
    "_"+locale.toString :: "_"+locale.getLanguage :: "" :: Nil
  }
  
  
  private def lookForClasses(places : List[String]) : Can[NodeSeq] = {
    val (controller, action) = places match {
      case ctl :: act :: _ => (ctl, act)
      case ctl :: _ => (ctl, "index")
      case Nil => ("default_template", "index")
    }
    val trans = List[String => String](n => n, n => camelCase(n))
    val toTry = trans.flatMap(f => (LiftRules.buildPackage("view") ::: ("lift.app.view" :: Nil)).map(_ + "."+f(controller)))
    
    first(toTry) {
      clsName =>
      try {
        tryo(List(classOf[ClassNotFoundException]), Empty) (Class.forName(clsName).asInstanceOf[Class[AnyRef]]).flatMap{
          c =>
          (c.newInstance match {
            case inst: InsecureLiftView => c.getMethod(action, null).invoke(inst, null)
            case inst: LiftView if inst.dispatch_&.isDefinedAt(action) => inst.dispatch_&(action)()
            case _ => Empty
          }) match {
            case null | Empty | None => Empty
            case n: Group => Full(n)
            case n: Elem => Full(n)
            case s: NodeSeq => Full(s)
            case Some(n: Group) => Full(n)
            case Some(n: Elem) => Full(n)
            case Some(n: NodeSeq) => Full(n)
            case Some(n: Seq[Node]) => Full(n)
            case Full(n: Group) => Full(n)
            case Full(n: Elem) => Full(n)
            case Full(n: NodeSeq) => Full(n)
            case Full(n: Seq[Node]) => Full(n)
            case _ => Empty
          }
        }
      } catch {
        case ite: java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[ResponseShortcutException]) => throw ite.getCause
        case re: ResponseShortcutException => throw re
        case _ => Empty
      }
    }
  }
}

