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
package net.liftweb.http;

import _root_.javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession, Cookie}
import _root_.javax.servlet.{ServletContext}
import _root_.java.net.URLDecoder
import _root_.scala.xml.{Node, NodeSeq,Group, Elem, MetaData, Null, XML, Comment, Text}
import _root_.scala.xml.transform._
import _root_.scala.actors._
import _root_.scala.actors.Actor._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.mapper.DB
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers
import _root_.net.liftweb.util.ActorPing
import _root_.java.util.{Locale, ResourceBundle}
import _root_.java.net.URL
import auth._
import js._
import _root_.javax.servlet._

/**
 * An implementation of HttpServlet.  Just drop this puppy into
 * your Java web container, do a little magic in web.xml, and
 * ta-da, you've got a scala-powered Servlet
 *
 */
class LiftServlet extends HttpServlet {
  private var servletContext: ServletContext = null

  def this(ctx: ServletContext) = {
    this()
    this.servletContext = ctx
  }

  override def getServletContext: ServletContext = servletContext

  override def destroy = {
    try {
      LiftRules.ending = true
      LiftRules.runUnloadHooks()
      Scheduler.snapshot // pause the Actor scheduler so we don't have threading issues
      Scheduler.shutdown
      ActorPing.shutdown
      Log.debug("Destroyed servlet")
      // super.destroy
    } catch {
      case e => Log.error("Servlet destruction failure",e)
    } finally {
      clearThread
    }
  }

  override def init = {
    try {
      LiftRules.ending = false
      LiftRules.addDispatchAfter({
          case r @ Req(mainPath :: subPath, suffx, _)
            if mainPath == LiftRules.ResourceServerPath =>
            ResourceServer.findResourceInClasspath(r, r.path.wholePath.drop(1))
        })
    } finally {
      clearThread
    }
  }

  def getLiftSession(request: Req, httpSession: HttpSession): LiftSession = {
    val wp = request.path.wholePath
    val cometSessionId =
    if (wp.length >= 3 && wp.head == LiftRules.cometPath)
    Full(wp(2))
    else
    Empty

    val ret = SessionMaster.getSession(httpSession, cometSessionId) match {
      case Full(ret) =>
        ret.lastServiceTime = millis // DO NOT REMOVE THIS LINE!!!!!
        ret

      case _ =>
        val ret = LiftSession(httpSession, request.contextPath, request.headers)
        ret.lastServiceTime = millis
        SessionMaster.addSession(ret)
        ret
    }

    ret.breakOutComet()
    ret
  }

  def service(req: HttpServletRequest,resp: HttpServletResponse, requestState: Req): Boolean = {
    try {
      def doIt: Boolean = {
        logTime("Service request ("+req.getMethod+") "+req.getRequestURI) {
          doService(req, resp, requestState)
        }
      }
      LiftRules.checkContinuations(req) match {
        case None => doIt
        case r if r eq null => doIt
        case r: LiftResponse => sendResponse(r.toResponse, resp, Empty) ; true
        case Some(r: LiftResponse) => sendResponse(r.toResponse, resp, Empty); true
        case _ => doIt
      }
    } catch {
      case e if e.getClass.getName.endsWith("RetryRequest") => throw e
      case e => Log.warn("Request for "+req.getRequestURI+" failed "+e.getMessage, e); throw e
    } finally {
      clearThread
    }
  }

  private def flatten(in: List[Any]): List[Any] = in match {
    case Nil => Nil
    case Some(x: AnyRef) :: xs => x :: flatten(xs)
    case Full(x: AnyRef) :: xs => x :: flatten(xs)
    case (lst: Iterable[_]) :: xs => lst.toList ::: flatten(xs)
    case (x: AnyRef) :: xs => x :: flatten(xs)
    case x :: xs => flatten(xs)
  }

  private def authPassed_?(req : Req) : Boolean = LiftRules.httpAuthProtectedResource.isDefinedAt(req.path) match {
    case true => val role = LiftRules.httpAuthProtectedResource(req.path)
      LiftRules.authentication.verified_?(req) match {
        case Full(r) =>
          // Check roles only ifthe resource is protected by Role.
          role.map(resRole => r.isChildOf(resRole)) openOr true
        case _ => false
      }
    case _ => true
  }


  /**
   * Service the HTTP request
   */
  def doService(request: HttpServletRequest, response: HttpServletResponse, requestState: Req): Boolean = {
    LiftRules.onBeginServicing.foreach(_(requestState))

    val statelessToMatch = requestState

    val resp: Can[LiftResponse] =
    // if the servlet is shutting down, return a 404
    if (LiftRules.ending) {
      LiftRules.notFoundOrIgnore(requestState, Empty)
    } else if (!authPassed_?(requestState)) {
      Full(LiftRules.authentication.unauthorizedResponse)
    } else
    // if the request is matched is defined in the stateless table, dispatch
    // it
    if (LiftRules.statelessDispatchTable.isDefinedAt(statelessToMatch)) {
      val f = LiftRules.statelessDispatchTable(statelessToMatch)
      f() match {
        case Full(v) => Full(LiftRules.convertResponse( (v, Nil, S.responseCookies, requestState) ))
        case Empty => LiftRules.notFoundOrIgnore(requestState, Empty)
        case f: Failure => Full(requestState.createNotFound(f))
      }
    } else
    // otherwise do a stateful response
    {
      val liftSession = getLiftSession(requestState, request.getSession)

      S.init(requestState, liftSession) {
        dispatchStatefulRequest(request, liftSession, requestState)
      }
    }

    LiftRules.onEndServicing.foreach(_(requestState, resp))

    resp match {
      case Full(cresp) =>
        val resp = cresp.toResponse

        logIfDump(requestState, resp)

        sendResponse(resp, response, Full(requestState))
        true

      case _ => false
    }
  }

  private def dispatchStatefulRequest(request: HttpServletRequest,
                                      liftSession: LiftSession,
                                      requestState: Req):
  Can[LiftResponse] =
  {
    val toMatch = requestState
    val dispatch: (Boolean, Can[LiftResponse]) =
    if (LiftRules.dispatchTable(request).isDefinedAt(toMatch)) {
      LiftSession.onBeginServicing.foreach(_(liftSession, requestState))
      val ret: (Boolean, Can[LiftResponse]) = try {
        val f = LiftRules.dispatchTable(request)(toMatch)
        f() match {
          case Full(v) =>
            (true, Full(LiftRules.convertResponse( (liftSession.checkRedirect(v), Nil,
                                                    S.responseCookies, requestState) )))

          case Empty =>
            (true, LiftRules.notFoundOrIgnore(requestState, Full(liftSession)))

          case f: Failure =>
            (true, Full(liftSession.checkRedirect(requestState.createNotFound(f))))
        }
      } finally {
        liftSession.notices = S.getNotices
      }
      LiftSession.onEndServicing.foreach(_(liftSession, requestState,
                                           ret._2))
      ret

    } else (false, Empty)

    val wp = requestState.path.wholePath

    val toTransform: Can[LiftResponse] =
    if (dispatch._1) dispatch._2
    else if (wp.length == 3 && wp.head == LiftRules.cometPath &&
             wp(2) == LiftRules.cometScriptName())
    LiftRules.serveCometScript(liftSession, requestState)
    else if ((wp.length >= 1) && wp.head == LiftRules.cometPath)
    handleComet(requestState, liftSession)
    else if (wp.length == 1 && wp.head == LiftRules.ajaxPath)
    handleAjax(liftSession, requestState)
    else if (wp.length == 2 && wp.head == LiftRules.ajaxPath &&
             wp(1) == LiftRules.ajaxScriptName())
    LiftRules.serveAjaxScript(liftSession, requestState)
    else liftSession.processRequest(requestState)

    toTransform.map(LiftRules.performTransform)
  }

  private def handleAjax(liftSession: LiftSession,
                         requestState: Req): Can[LiftResponse] =
  {
    LiftRules.cometLogger.debug("AJAX Request: "+liftSession.uniqueId+" "+requestState.params)
    LiftSession.onBeginServicing.foreach(_(liftSession, requestState))
    val ret = try {
      val what = flatten(liftSession.runParams(requestState))

      val what2 = what.flatMap{case js: JsCmd => List(js)
        case n: NodeSeq => List(n)
        case js: JsCommands => List(js)
        case r: LiftResponse => List(r)
        case s => Nil
      }

      val ret: LiftResponse = what2 match {
        case (n: Node) :: _ => XmlResponse(n)
        case (ns: NodeSeq) :: _ => XmlResponse(Group(ns))
        case (r: LiftResponse) :: _ => r
        case (js: JsCmd) :: xs  => (JsCommands(S.noticesToJsCmd::Nil) & ((js :: xs).flatMap{case js: JsCmd => List(js) case _ => Nil}.reverse)).toResponse
        case _ => JsCommands(S.noticesToJsCmd :: JsCmds.Noop :: Nil).toResponse
      }

      LiftRules.cometLogger.debug("AJAX Response: "+liftSession.uniqueId+" "+ret)
      Full(ret)
    } finally {
      liftSession.updateFunctionMap(S.functionMap)
    }
    LiftSession.onEndServicing.foreach(_(liftSession, requestState, ret))
    ret
  }

  class ContinuationActor(request: Req, sessionActor: LiftSession, actors: List[(CometActor, Long)]) extends Actor {
    private var answers: List[AnswerRender] = Nil
    val seqId = CometActor.next

    def act = loop {
      react {
        case BeginContinuation =>
          val mySelf = self
          val sendItToMe: AnswerRender => Unit = ah => mySelf ! (seqId, ah)

          actors.foreach{case (act, when) => act ! Listen(when, ListenerId(seqId), sendItToMe)}

        case (theId: Long, ar: AnswerRender) =>
          answers = ar :: answers
          ActorPing.schedule(this, BreakOut, TimeSpan(5))

        case BreakOut =>
          actors.foreach{case (act, _) => act ! Unlisten}
          LiftRules.resumeRequest(
            S.init(request, sessionActor)
            (LiftRules.performTransform(
                convertAnswersToCometResponse(sessionActor,
                                              answers.toArray, actors))),
                                                request.request)

          sessionActor.exitComet(this)
          this.exit()

        case _ =>
      }
    }

    override def toString = "Actor dude "+seqId
  }

  private object BeginContinuation

  private lazy val cometTimeout: Long = (LiftRules.cometRequestTimeout openOr 120) * 1000L

  private def setupContinuation(requestState: Req, sessionActor: LiftSession, actors: List[(CometActor, Long)]): Nothing = {
    val cont = new ContinuationActor(requestState, sessionActor, actors)
    cont.start

    cont ! BeginContinuation

    sessionActor.enterComet(cont)

    ActorPing.schedule(cont, BreakOut, TimeSpan(cometTimeout))

    LiftRules.doContinuation(requestState.request, cometTimeout + 2000L)
  }

  private def handleComet(requestState: Req, sessionActor: LiftSession): Can[LiftResponse] = {
    val actors: List[(CometActor, Long)] = requestState.params.toList.flatMap{case (name, when) => sessionActor.getAsyncComponent(name).toList.map(c => (c, toLong(when)))}

    if (actors.isEmpty) Full(new JsCommands(JsCmds.RedirectTo(LiftRules.noCometSessionPage) :: Nil).toResponse)
    else LiftRules.checkContinuations(requestState.request) match {
      case Some(null) =>
        setupContinuation(requestState, sessionActor, actors)

      case _ =>
        handleNonContinuationComet(requestState, sessionActor, actors)
    }
  }

  private def convertAnswersToCometResponse(sessionActor: LiftSession, ret: Seq[AnswerRender], actors: List[(CometActor, Long)]): LiftResponse = {
    val ret2 = ret.toList
    val jsUpdateTime = ret2.map(ar => "lift_toWatch['"+ar.who.uniqueId+"'] = '"+ar.when+"';").mkString("\n")
    val jsUpdateStuff = ret2.map(ar => ar.response.toJavaScript(sessionActor, ar.displayAll))

    actors foreach(_._1 ! ClearNotices)

    (new JsCommands(JsCmds.Run(jsUpdateTime) :: jsUpdateStuff)).toResponse
  }

  private def handleNonContinuationComet(requestState: Req, sessionActor: LiftSession, actors: List[(CometActor, Long)]): Can[LiftResponse] = {

    LiftRules.cometLogger.debug("Comet Request: "+sessionActor.uniqueId+" "+requestState.params)

    sessionActor.enterComet(self)
    try {
      val seqId = CometActor.next

      def drainTheSwamp(len: Long, in: List[AnswerRender]): List[AnswerRender] = { // remove any message from the current thread's inbox
        receiveWithin(len) {
          case TIMEOUT =>
            in

          case (theId: Long, ar: AnswerRender) if theId == seqId =>
            drainTheSwamp(0, ar :: in)

          case BreakOut => in

          case s =>
            Log.trace("Drained "+s)
            drainTheSwamp(len, in)
        }
      }

      val mySelf = self

      // the function that sends an AnswerHandler to me
      val sendItToMe: AnswerRender => Unit = ah => mySelf ! (seqId, ah)

      actors.foreach{case (act, when) => act ! Listen(when, ListenerId(seqId), sendItToMe)}

      val ret = drainTheSwamp(cometTimeout, Nil)

      actors.foreach{case (act, _) => act ! Unlisten}

      val ret2 = drainTheSwamp(5L, ret)

      Full(convertAnswersToCometResponse(sessionActor, ret2, actors))
    } finally {
      sessionActor.exitComet(self)
    }
  }

  val dumpRequestResponse = Props.getBool("dump.request.response", false)

  private def logIfDump(request: Req, response: BasicResponse) {
    if (dumpRequestResponse) {
      val toDump = request.uri+"\n"+
      request.params + "\n"+
      response.headers+"\n"+
      (
        response match {
          case InMemoryResponse(data, _, _, _) => new String(data, "UTF-8")
          case _ => "data"
        }
      )

      Log.info(toDump)
    }
  }

  /**
   * Sends the {@code HttpServletResponse} to the browser using data from the
   * {@link Response} and {@link Req}.
   */
  def sendResponse(resp: BasicResponse, response: HttpServletResponse, request: Can[Req]) {
    def fixHeaders(headers : List[(String, String)]) = headers map ((v) => v match {
        case ("Location", uri) => (v._1, response.encodeURL(request map (_ updateWithContextPath(uri)) openOr uri))
        case _ => v
      })

    def pairFromRequest(in: Can[Req]): (Can[Req], Can[String]) = {
      val acceptHeader = for (req <- in;
                              innerReq <- Can.legacyNullTest(req.request);
                              accept <- Can.legacyNullTest(innerReq.getHeader("Accept"))) yield accept

      (in, acceptHeader)
    }


    val len = resp.size
    // insure that certain header fields are set
    val header = insureField(fixHeaders(resp.headers), List(("Content-Type",
                                                             LiftRules.determineContentType( pairFromRequest(request) )),
                                                            ("Content-Length", len.toString)))

    LiftRules._beforeSend.foreach(f => tryo(f(resp, response, header, request)))
    // set the cookies
    resp.cookies.foreach(cookie => response.addCookie(cookie))

    // send the response
    header.elements.foreach {case (name, value) => response.setHeader(name, value)}
    response setStatus resp.code

    resp match {
      case InMemoryResponse(bytes, _, _, _) =>
        response.getOutputStream.write(bytes)

      case StreamingResponse(stream, endFunc, _, _, _, _) =>
        try {
          var len = 0
          val ba = new Array[Byte](8192)
          val os = response.getOutputStream
          len = stream.read(ba)
          while (len >= 0) {
            if (len > 0) os.write(ba, 0, len)
            len = stream.read(ba)
          }

        } finally {
          endFunc()
        }
    }

    LiftRules._afterSend.foreach(f => tryo(f(resp, response, header, request)))
  }

  /**
   * Remove any thread-local associations
   */
  def clearThread: Unit = {
    Actor.clearSelf
    DB.clearThread
  }

}

trait LiftFilterTrait {
  def actualServlet: LiftServlet

  def doFilter(req: ServletRequest, res: ServletResponse,chain: FilterChain) =
  {
    RequestVarHandler(
      (req, res) match {
        case (httpReq: HttpServletRequest, httpRes: HttpServletResponse) =>
          LiftRules.early.foreach(_(httpReq))

          val session = Req(httpReq, LiftRules.rewriteTable(httpReq), System.nanoTime)

          URLRewriter.doWith(url => LiftRules.urlDecorate(httpRes.encodeURL(url))) {
            if (isLiftRequest_?(session) && actualServlet.service(httpReq, httpRes, session)) {
            } else {
              chain.doFilter(req, res)
            }
          }

        case _ => chain.doFilter(req, res)
      })
  }

  def isLiftRequest_?(session: Req): Boolean
}

class LiftFilter extends Filter with LiftFilterTrait
{
  //The variable holds the current ServletContext (we need it for request URI - handling
  private var context: ServletContext = null
  var actualServlet: LiftServlet = _



  //We need to capture the ServletContext on init
  def init(config:FilterConfig) {
    context = config.getServletContext

    LiftRules.setContext(context)
    bootLift(Can.legacyNullTest(config.getInitParameter("bootloader")))

    actualServlet = new LiftServlet(context)
    actualServlet.init
  }

  //And throw it away on destruction
  def destroy {
    context = null
    if (actualServlet != null) {
      actualServlet.destroy
      actualServlet = null
    }
  }

  def bootLift(loader : Can[String]) : Unit =
  {
    try
    {
      val b : Bootable = loader.map(b => Class.forName(b).newInstance.asInstanceOf[Bootable]) openOr DefaultBootstrap

      b.boot
      postBoot
    } catch {
      case e => Log.error("Failed to Boot", e); None
    }
  }

  private def postBoot {
    try {
      ResourceBundle getBundle (LiftRules.liftCoreResourceName)
    } catch {
      case _ => Log.error("LiftWeb core resource bundle for locale " + Locale.getDefault() + ", was not found ! ")
    }
  }


  //This function tells you wether a resource exists or not, could probably be better
  private def liftHandled(in: String): Boolean = (in.indexOf(".") == -1) || in.endsWith(".html") || in.endsWith(".xhtml") ||
  in.endsWith(".htm") ||
  in.endsWith(".xml") || in.endsWith(".liftjs") || in.endsWith(".liftcss")

  def isLiftRequest_?(session: Req): Boolean = {
    if (LiftRules.isLiftRequest_?.isDefinedAt(session)) LiftRules.isLiftRequest_?(session)
    else session.path.endSlash || (session.path.wholePath.takeRight(1) match {case Nil => true case x :: xs => liftHandled(x)}) ||
    context.getResource(session.uri) == null
  }

}

