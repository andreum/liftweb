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

import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.http.js.JSArtifacts
import _root_.net.liftweb.http.js.jquery._
import _root_.scala.xml._
import _root_.scala.collection.mutable.{ListBuffer}
import _root_.java.util.{Locale, TimeZone}
import _root_.javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession, Cookie}
import auth._
import _root_.javax.servlet.{ServletContext}
import _root_.java.io.{InputStream, ByteArrayOutputStream, BufferedReader, StringReader}
import js._
import JE._

object LiftRules {
  val noticesContainerId = "lift__noticesContainer__"

  type DispatchPf = PartialFunction[Req, () => Can[LiftResponse]];
  type RewritePf = PartialFunction[RewriteRequest, RewriteResponse]
  type TemplatePf = PartialFunction[Req,() => Can[NodeSeq]]
  type SnippetPf = PartialFunction[List[String], NodeSeq => NodeSeq]
  type LiftTagPF = PartialFunction[(String, Elem, MetaData, NodeSeq, String), NodeSeq]
  type URINotFoundPF = PartialFunction[(Req, Can[Failure]), LiftResponse]
  type URLDecorator = PartialFunction[String, String]
  type SnippetDispatchPf = PartialFunction[String, DispatchSnippet]
  type ViewDispatchPf = PartialFunction[List[String], LiftView]
  type HttpAuthProtectedResourcePf = PartialFunction[ParsePath, Can[Role]]

  /**
   * A partial function that allows the application to define requests that should be
   * handled by lift rather than the default servlet handler
   */
  type LiftRequestPf = PartialFunction[Req, Boolean]

  private var _early: List[(HttpServletRequest) => Any] = Nil
  private[http] var _beforeSend: List[(BasicResponse, HttpServletResponse, List[(String, String)], Can[Req]) => Any] = Nil

  def appendBeforeSend(f: (BasicResponse, HttpServletResponse, List[(String, String)], Can[Req]) => Any) {
    _beforeSend = _beforeSend ::: List(f)
  }

  /**
   * Defines the resources that are protected by authentication and authorization. If this function
   * is notdefined for the input data, the resource is considered unprotected ergo no authentication
   * is performed. If this function is defined and returns a Full can, it means that this resource
   * is protected by authentication,and authenticated subjed must be assigned to the role returned by
   * this function or to a role that is child-of this role. If this function returns Empty it means that
   * this resource is protected by authentication but no authorization is performed meaning that roles are
   * not verified.
   */
  var httpAuthProtectedResource: HttpAuthProtectedResourcePf = Map.empty

  /**
   * The HTTP authentication mechanism that ift will perform. See <i>LiftRules.protectedResource</i>
   */
  var authentication : HttpAuthentication = NoAuthentication

  /**
   * A function that takes the HTTPSession and the contextPath as parameters
   * and returns a LiftSession reference. This can be used in cases subclassing
   * LiftSession is necessary.
   */
  var sessionCreator: (HttpSession,  String, List[(String, String)]) => LiftSession = {
    case (httpSession, contextPath, headers) => new LiftSession(contextPath, httpSession.getId, httpSession, headers)
  }

  /**
   * The path to handle served resources
   */
  var ResourceServerPath = "classpath"

  /**
   * Holds the JS library specific UI artifacts. By efault it uses JQuery's artifacts
   */
  var jsArtifacts: JSArtifacts = JQueryArtifacts

  /**
   * Use this PartialFunction to to automatically add static URL parameters
   * to any URL reference from the markup of Ajax request.
   */
  var urlDecorate: URLDecorator = {case arg => arg}

  private[http] var _afterSend: List[(BasicResponse, HttpServletResponse, List[(String, String)], Can[Req]) => Any] = Nil

  def appendAfterSend(f: (BasicResponse, HttpServletResponse, List[(String, String)], Can[Req]) => Any) {
    _afterSend = _afterSend ::: List(f)
  }

  /**
   * Calculate the Comet Server (by default, the server that
   * the request was made on, but can do the multi-server thing
   * as well)
   */
  var cometServer: () => String = () => S.contextPath

  /**
   * The maximum concurrent requests.  If this number of
   * requests are being serviced for a given session, messages
   * will be sent to all Comet requests to terminate
   */
  var maxConcurrentRequests = 2

  /**
   * A partial function that determines content type based on an incoming
   * Req and Accept header
   */
  var determineContentType:
  PartialFunction[(Can[Req], Can[String]), String] = {
    case (_, Full(accept)) if this.useXhtmlMimeType && accept.toLowerCase.contains("application/xhtml+xml") =>
       "application/xhtml+xml"

    case _ => "text/html"
  }


  /**
   * Hooks to be run when LiftServlet.destroy is called.
   */
  val unloadHooks = new ListBuffer[() => Unit]()

  /**
   * For each unload hook registered, run them during destroy()
   */
  def runUnloadHooks() {
    unloadHooks.foreach(_())
  }

  /**
   * Adds a function to get run during Servlet.destroy
   */
  def addUnloadHook(f: () => Unit) {
    unloadHooks += f
  }

  /**
   * The maximum allowed size of a complete mime multi-part POST.  Default
   * 8MB
   */
  var maxMimeSize: Long = 8 * 1024 * 1024

  /**
   * Should pages that are not found be passed along the servlet chain to the
   * next handler?
   */
  var passNotFoundToChain = true

  /**
   * The maximum allowed size of a single file in a mime multi-part POST.
   * Default 7MB
   */
  var maxMimeFileSize: Long = 7 * 1024 * 1024

  /**
   * The function referenced here is called if there's a localization lookup failure
   */
  var localizationLookupFailureNotice: Can[(String, Locale) => Unit] = Empty

  /**
   * The default location to send people if SiteMap access control fails
   */
  var siteMapFailRedirectLocation: List[String] = List()

  private[http] def notFoundOrIgnore(requestState: Req, session: Can[LiftSession]): Can[LiftResponse] = {
    if (passNotFoundToChain) Empty
    else session match {
      case Full(session) => Full(session.checkRedirect(requestState.createNotFound))
      case _ => Full(requestState.createNotFound)
    }
  }

  var liftTagProcessing: LiftTagPF = Map.empty

  /**
   * If you don't want lift to send the application/xhtml+xml mime type to those browsers
   * that understand it, then set this to {@code false}
   */
 var useXhtmlMimeType: Boolean = true


  private def _stringToXml(s: String): NodeSeq = Text(s)

  /**
   * A function that defines how a String should be converted to XML
   * for the localization stuff.  By default, Text(s) is returned,
   * but you can change this to attempt to parse the XML in the String and
   * return the NodeSeq.
   */
  var localizeStringToXml: String => NodeSeq = _stringToXml _

  /**
   * The base name of the resource bundle
   */
  var resourceName = "lift"

  /**
   * The base name of the resource bundle of the lift core code
   */
  var liftCoreResourceName = "i18n.lift-core"

  /**
   * Where to send the user if there's no comet session
   */
  var noCometSessionPage = "/"

  /**
   * Put a function that will calculate the request timeout based on the
   * incoming request.
   */
  var calcRequestTimeout: Can[Req => Int] = Empty

  /**
   * If you want the standard (non-AJAX) request timeout to be something other than
   * 10 seconds, put the value here
   */
  var stdRequestTimeout: Can[Int] = Empty

  /**
   * If you want the AJAX request timeout to be something other than 120 seconds, put the value here
   */
  var cometRequestTimeout: Can[Int] = Empty

  /**
   * Meta information for the notices that are applied via Ajax response
   */
  var ajaxNoticeMeta: Can[AjaxMessageMeta] = Empty

  /**
   * Meta information for the warnings that are applied via Ajax response
   */
  var ajaxWarningMeta: Can[AjaxMessageMeta] = Empty

  /**
   * Meta information for the errors that are applied via Ajax response
   */
  var ajaxErrorMeta: Can[AjaxMessageMeta] = Empty

  /**
   * The dispatcher that takes a Snippet and converts it to a
   * DispatchSnippet instance
   */
  var snippetDispatch: SnippetDispatchPf = Map.empty

  /**
   * Change this variable to set view dispatching
   */
  var viewDispatch: ViewDispatchPf = Map.empty

  def snippet(name: String): Can[DispatchSnippet] =
  if (snippetDispatch.isDefinedAt(name)) Full(snippetDispatch(name))
  else Empty

  /**
   * If the request times out (or returns a non-Response) you can
   * intercept the response here and create your own response
   */
  var requestTimedOut: Can[(Req, Any) => Can[LiftResponse]] = Empty

  def early = {
    // test_boot
    _early
  }

  /**
   * A function that takes the current HTTP request and returns the current
   */
  var timeZoneCalculator: Can[HttpServletRequest] => TimeZone =
  defaultTimeZoneCalculator _

  def defaultTimeZoneCalculator(request: Can[HttpServletRequest]): TimeZone =
  TimeZone.getDefault


  /**
   * How many times do we retry an Ajax command before calling it a failure?
   */
  var ajaxRetryCount: Can[Int] = Empty

  /**
   * The JavaScript to execute at the begining of an
   * Ajax request (for example, showing the spinning working thingy)
   */
  var ajaxStart: Can[() => JsCmd] = Empty

  /**
   * The JavaScript to execute at the end of an
   * Ajax request (for example, removing the spinning working thingy)
   */
  var ajaxEnd: Can[() => JsCmd] = Empty

  /**
   * The default action to take when the JavaScript action fails
   */
  var ajaxDefaultFailure: Can[() => JsCmd] =
  Full(() => JsCmds.Alert(S.??("The server cannot be contacted at this time")))

  /**
   * A function that takes the current HTTP request and returns the current
   */
  var localeCalculator: Can[HttpServletRequest] => Locale = defaultLocaleCalculator _

  def defaultLocaleCalculator(request: Can[HttpServletRequest]) = request.flatMap(_.getLocale() match {case null => Empty case l: Locale => Full(l)}).openOr(Locale.getDefault())


  val (hasContinuations_?, contSupport, getContinuation, getObject, setObject, suspend, resume) = {
    try {
      val cc = Class.forName("org.mortbay.util.ajax.ContinuationSupport")
      val meth = cc.getMethod("getContinuation", classOf[HttpServletRequest], classOf[AnyRef])
      val cci = Class.forName("org.mortbay.util.ajax.Continuation")
      val getObj = cci.getMethod("getObject")
      val setObj = cci.getMethod("setObject", classOf[AnyRef])
      val suspend = cci.getMethod("suspend", _root_.java.lang.Long.TYPE)
      val resume = cci.getMethod("resume")
      (true, (cc), (meth), (getObj), (setObj), (suspend), resume)
    } catch {
      case e => (false, null, null, null, null, null, null)
    }
  }

  def resumeRequest(what: AnyRef, req: HttpServletRequest) {
    val cont = getContinuation.invoke(contSupport, req, LiftRules)
    setObject.invoke(cont, what)
    resume.invoke(cont)
  }

  def doContinuation(req: HttpServletRequest, timeout: Long): Nothing = {
    try {
      val cont = getContinuation.invoke(contSupport, req, LiftRules)
      Log.trace("About to suspend continuation")
      suspend.invoke(cont, new _root_.java.lang.Long(timeout))
      throw new Exception("Bail")
    } catch {
      case e: _root_.java.lang.reflect.InvocationTargetException if e.getCause.getClass.getName.endsWith("RetryRequest") =>
        throw e.getCause
    }
  }

  def checkContinuations(req: HttpServletRequest): Option[Any] = {
    if (!hasContinuations_?) None
    else {
      val cont = getContinuation.invoke(contSupport, req, LiftRules)
      val ret = getObject.invoke(cont)
      setObject.invoke(cont, null)
      Some(ret)
    }
  }

  private var _sitemap: Can[SiteMap] = Empty

  def setSiteMap(sm: SiteMap) {
    _sitemap = Full(sm)
    val rewriters = sm.menus.map(_.loc).
    flatMap(_.rewritePf).
    foldLeft[RewritePf](Map.empty)(_ orElse _)

    addRewriteAfter(rewriters)
  }

  def siteMap: Can[SiteMap] = _sitemap

  def appendEarly(f: HttpServletRequest => Any) = _early = _early ::: List(f)

  var ending = false
  private case object Never

  private def rpf[A,B](in: List[PartialFunction[A,B]], last: PartialFunction[A,B]): PartialFunction[A,B] = in match {
    case Nil => last
    case x :: xs => x orElse rpf(xs, last)
  }

  var statelessDispatchTable: DispatchPf = Map.empty

  def dispatchTable(req: HttpServletRequest): DispatchPf = {
    req match {
      case null => dispatchTable_i
      case _ => SessionMaster.getSession(req, Empty) match {
          case Full(s) => S.initIfUninitted(s) {
              rpf(S.highLevelSessionDispatchList.map(_.dispatch), dispatchTable_i)
            }
          case _ => dispatchTable_i
        }
    }
  }

  def rewriteTable(req: HttpServletRequest): RewritePf = {
    req match {
      case null => rewriteTable_i
      case _ => SessionMaster.getSession(req, Empty) match {
          case Full(s) => S.initIfUninitted(s) {
              rpf(S.sessionRewriter.map(_.rewrite), rewriteTable_i)
            }
          case _ => rewriteTable_i
        }
    }
  }

  def snippetTable: SnippetPf = snippetTable_i

  def templateTable(req: HttpServletRequest): TemplatePf = {
    req match {
      case null => templateTable_i
      case _ => SessionMaster.getSession(req, Empty) match {
          case Full(s) => S.initIfUninitted(s) {
              rpf(S.sessionTemplater.map(_.template), templateTable_i)
            }
          case _ => templateTable_i
        }
    }
  }

  var ajaxPath = "ajax_request"

  var cometPath = "comet_request"

  var calcCometPath: String => JsExp =
  prefix => Str(prefix + "/" + cometPath + "/") +
  JsRaw("Math.floor(Math.random() * 100000000000)") +
  Str(S.session.map(s => "/"+s.uniqueId) openOr "")


  /**
   * The default way of calculating the context path
   */
  def defaultCalcContextPath(request: HttpServletRequest): Can[String] = {
    request.getHeader("X-Lift-ContextPath") match {
      case null => Empty
      case s if s.trim == "/" => Full("")
      case s => Full(s.trim)
    }
  }

  /**
   * If there is an alternative way of calculating the context path
   * (by default inspecting the X-Lift-ContextPath header)
   */
  var calculateContextPath: HttpServletRequest => Can[String] =
  defaultCalcContextPath _

  private var _context: ServletContext = _

  def context: ServletContext = synchronized {_context}

  def setContext(in: ServletContext): Unit =  synchronized {
    if (in ne _context) {
      // Helpers.setResourceFinder(in.getResource)
      _context = in
      //   performBoot(in)
    }
  }

  private var otherPackages: List[String] = Nil

  def buildPackage(end: String)  = synchronized (otherPackages.map(_+"."+end))

  def addToPackages(what: String) {synchronized {otherPackages = what :: otherPackages}}

  private val defaultFinder = getClass.getResource _
  private def resourceFinder(name: String): _root_.java.net.URL = _context.getResource(name)

  def getResource(name: String): Can[_root_.java.net.URL] = resourceFinder(name) match {case null => defaultFinder(name) match {case null => Empty; case s => Full(s)} ; case s => Full(s)}
  def getResourceAsStream(name: String): Can[_root_.java.io.InputStream] = getResource(name).map(_.openStream)
  def loadResource(name: String): Can[Array[Byte]] = getResourceAsStream(name).map{
    stream =>
    val buffer = new Array[Byte](2048)
    val out = new ByteArrayOutputStream
    def reader {
      val len = stream.read(buffer)
      if (len < 0) return
      else if (len > 0) out.write(buffer, 0, len)
      reader
    }
    reader
    stream.close
    out.toByteArray
  }
  def loadResourceAsXml(name: String): Can[NodeSeq] = loadResourceAsString(name).flatMap(s => PCDataXmlParser(s))
  def loadResourceAsString(name: String): Can[String] = loadResource(name).map(s => new String(s, "UTF-8"))



  def finder(name: String): Can[InputStream] = {
    LiftRules.context match {
      case null => Empty
      case c => c.getResourceAsStream(name) match {
          case null => Empty
          case s => Full(s)
        }
    }
  }

  /**
   * Get the partial function that defines if a request should be handled by
   * the application (rather than the default servlet handler)
   */
  def isLiftRequest_? : LiftRequestPf = i_isLiftRequest_?

  /**
   * Append a partial function to the list of interceptors to test
   * if the request should be handled by lift
   */
  def addLiftRequest(what: LiftRequestPf) {i_isLiftRequest_? = i_isLiftRequest_? orElse what}

  private var i_isLiftRequest_? : LiftRequestPf = Map.empty

  private var dispatchTable_i : DispatchPf = Map.empty

  private var rewriteTable_i : RewritePf = Map.empty

  private var templateTable_i: TemplatePf = Map.empty

  private var snippetTable_i: SnippetPf = Map.empty

  var cometLoggerBuilder: () => LiftLogger = () => {
    val ret = LogBoot.loggerByName("comet_trace")
    ret.level = LiftLogLevels.Off
    ret
  }

  lazy val cometLogger: LiftLogger = cometLoggerBuilder()

  def addSnippetBefore(pf: SnippetPf) = {
    snippetTable_i = pf orElse snippetTable_i
    snippetTable_i
  }

  def addSnippetAfter(pf: SnippetPf) = {
    snippetTable_i = snippetTable_i orElse pf
    snippetTable_i
  }

  def addTemplateBefore(pf: TemplatePf) = {
    templateTable_i = pf orElse templateTable_i
    templateTable_i
  }

  def addTemplateAfter(pf: TemplatePf) = {
    templateTable_i = templateTable_i orElse pf
    templateTable_i
  }

  def addRewriteBefore(pf: RewritePf) = {
    rewriteTable_i = pf orElse rewriteTable_i
    rewriteTable_i
  }

  def addRewriteAfter(pf: RewritePf) = {
    rewriteTable_i = rewriteTable_i orElse pf
    rewriteTable_i
  }

  def addDispatchBefore(pf: DispatchPf) = {
    dispatchTable_i = pf orElse dispatchTable_i
    dispatchTable_i
  }

  def addDispatchAfter(pf: DispatchPf) = {
    dispatchTable_i = dispatchTable_i orElse pf
    dispatchTable_i
  }

  def addHttpAuthProtectedResource(pr: HttpAuthProtectedResourcePf) = {
    httpAuthProtectedResource = httpAuthProtectedResource orElse pr
    httpAuthProtectedResource
  }

  /**
   * Takes a Node, headers, cookies, and a session and turns it into an XhtmlResponse.
   */
  private def cvt(ns: Node, headers: List[(String, String)], cookies: List[Cookie], session: Req) =
  convertResponse((XhtmlResponse(Group(session.fixHtml(ns)),
                                 ResponseInfo.docType(session),
                                 headers, cookies, 200), headers, cookies, session))

  var defaultHeaders: PartialFunction[(NodeSeq, Req), List[(String, String)]] = {
    case _ => List(("Expires", "0"))
  }

  def performTransform(in: LiftResponse): LiftResponse =
  responseTransformers.foldLeft(in){
    case (in, pf: PartialFunction[LiftResponse, LiftResponse]) =>
      if (pf.isDefinedAt(in)) pf(in) else in
    case (in, f) => f(in)
  }

  var responseTransformers: List[LiftResponse => LiftResponse] =
  Nil

  /**
   * convertResponse is a PartialFunction that reduces a given Tuple4 into a
   * LiftResponse that can then be sent to the browser.
   */
  var convertResponse: PartialFunction[(Any, List[(String, String)], List[Cookie], Req), LiftResponse] = {
    case (r: LiftResponse, _, _, _) => r
    case (ns: Group, headers, cookies, session) => cvt(ns, headers, cookies, session)
    case (ns: Node, headers, cookies, session) => cvt(ns, headers, cookies, session)
    case (ns: NodeSeq, headers, cookies, session) => cvt(Group(ns), headers, cookies, session)
    case (SafeNodeSeq(n), headers, cookies, session) => cvt(Group(n), headers, cookies, session)

    case (Full(o), headers, cookies, session) => convertResponse( (o, headers, cookies, session) )

    case (Some(o), headers, cookies, session) => convertResponse( (o, headers, cookies, session) )
    case (bad, _, _, session) => session.createNotFound
  }

  /**
   * Set a snippet failure handler here.  The class and method for the snippet are passed in
   */
  var snippetFailedFunc: List[SnippetFailure => Unit] = logSnippetFailure _ :: Nil

  private def logSnippetFailure(sf: SnippetFailure) = Log.warn("Snippet Failure: "+sf)

  case class SnippetFailure(page: String, typeName: Can[String], failure: SnippetFailures.Value)

  object SnippetFailures extends Enumeration {
    val NoTypeDefined = Value(1, "No Type Defined")
    val ClassNotFound = Value(2, "Class Not Found")
    val StatefulDispatchNotMatched = Value(3, "Stateful Snippet: Dispatch Not Matched")
    val MethodNotFound = Value(4, "Method Not Found")
    val NoNameSpecified = Value(5, "No Snippet Name Specified")
  }

  /**
   * The function that deals with how exceptions are presented to the user during processing
   * of an HTTP request.  Put a new function here to change the behavior.
   *
   * The function takes the Req and the Exception and returns a LiftResponse that's
   * sent to the browser.
   */
  var logAndReturnExceptionToBrowser: (Req, Throwable) => LiftResponse = showException

  /**
   * The partial function (pattern matching) for handling converting an exception to something to
   * be sent to the browser depending on the current RunMode (development, etc.)
   *
   * The best thing to do is browserResponseToException = { case (...) => } orElse browserResponseToException
   * so that your response over-rides the default, but the processing falls through to the default.
   */
  var browserResponseToException: PartialFunction[(Props.RunModes.Value, Req, Throwable), LiftResponse] = {
    case (Props.RunModes.Development, r, e) =>
      XhtmlResponse((<html><body>Exception occured while processing {r.uri}
              <pre>{
                  _showException(e)
                }</pre></body></html>),ResponseInfo.docType(r), List("Content-Type" -> "text/html"), Nil, 500)

    case (_, r, e) =>
      XhtmlResponse((<html><body>Something unexpected happened while serving the page at {r.uri}
                           </body></html>),ResponseInfo.docType(r), List("Content-Type" -> "text/html"), Nil, 500)
  }

  /**
   * The partial function for defining the behavior of what happens when
   * URI is invalid and you're not using a site map
   *
   * It is strongly recommended to use partial functions composition like:
   * uriNotFound = {case (...) => ...} orElse uriNotFound if the pattern used is not exhaustive
   */
  var uriNotFound: URINotFoundPF = {
    case (r, _) => Req.defaultCreateNotFound(r)
  }


  /**
   * A utility method to convert an exception to a string of stack traces
   * @param le the exception
   *
   * @return the stack trace
   */
  def _showException(le: Throwable): String = {
    val ret = "Message: "+le.toString+"\n\t"+
    le.getStackTrace.map(_.toString).mkString("\n\t") + "\n"

    val also = le.getCause match {
      case null => ""
      case sub: Throwable => "\nCaught and thrown by:\n"+ _showException(sub)
    }

    ret + also
  }

  private def showException(r: Req, e: Throwable): LiftResponse = {
    Log.error("Exception being returned to browser when processing "+r, e)
    browserResponseToException(Props.mode, r, e)
  }

  /**
   * Modifies the root relative paths from the css url-s
   *
   * @param path - the path of the css resource
   * @prefix - the prefix to be added on the root relative paths. If this is Empty
   * 	       the prefix will be the application context path.
   */
  def fixCSS(path: List[String], prefix: Can[String]) {

    val liftReq: LiftRules.LiftRequestPf = new LiftRules.LiftRequestPf {
      def isDefinedAt(r: Req): Boolean = {
        r.path.partPath == path
      }
      def apply(r: Req): Boolean = {
        r.path.partPath == path
      }
    }

    val cssFixer: LiftRules.DispatchPf = new LiftRules.DispatchPf {
      def isDefinedAt(r: Req): Boolean = {
        r.path.partPath == path
      }
      def apply(r: Req): () => Can[LiftResponse] = {
        val cssPath = path.mkString("/", "/", ".css")
        val css = LiftRules.loadResourceAsString(cssPath);

        () => {
          css.map(str => CSSHelpers.fixCSS(new BufferedReader(
                new StringReader(str)), prefix openOr (S.contextPath)) match {
              case (Full(c), _) => CSSResponse(c)
              case (_, input) => {
                  Log.warn("Fixing " + cssPath + " failed");
                  CSSResponse(input)
                }
            })
        }
      }
    }
    LiftRules.addDispatchBefore(cssFixer);
    LiftRules.addLiftRequest(liftReq);
  }

  var onBeginServicing: List[Req => Unit] = Nil
  var onEndServicing: List[(Req, Can[LiftResponse]) => Unit] = Nil

  var autoIncludeComet: LiftSession => Boolean =
  session => true

  var autoIncludeAjax: LiftSession => Boolean =
  session => true

  var renderAjaxScript: LiftSession => JsCmd =
  session => JsCmds.Run("""
var lift_ajaxQueue = [];
var lift_ajaxInProcess = null;
var lift_ajaxShowing = false;
var lift_ajaxRetryCount = """+
                        (LiftRules.ajaxRetryCount openOr 3)+
                        """

function lift_ajaxHandler(theData, theSuccess, theFailure) {
  var toSend = {retryCnt: 0};
  toSend.when = (new Date()).getTime();
  toSend.theData = theData;
  toSend.onSuccess = theSuccess;
  toSend.onFailure = theFailure;

  lift_ajaxQueue.push(toSend);
  lift_ajaxQueueSort();
  lift_doAjaxCycle();
  return false; // buttons in forms don't trigger the form
}

function lift_ajaxQueueSort() {
  lift_ajaxQueue.sort(function (a,b) {return a.when - b.when;});
}

function lift_defaultFailure() {
"""+
                        (LiftRules.ajaxDefaultFailure.map(_().toJsCmd) openOr "")+
                        """
}

function lift_startAjax() {
  lift_ajaxShowing = true;
"""+
                        (LiftRules.ajaxStart.map(_().toJsCmd) openOr "")+
                        """
}

function lift_endAjax() {
  lift_ajaxShowing = false;
"""+
                        (LiftRules.ajaxEnd.map(_().toJsCmd) openOr "")+
                        """
}

function lift_testAndShowAjax() {
  if (lift_ajaxShowing && lift_ajaxQueue.length == 0 &&
      lift_ajaxInProcess == null) {
   lift_endAjax();
      } else if (!lift_ajaxShowing && (lift_ajaxQueue.length > 0 ||
     lift_ajaxInProcess != null)) {
   lift_startAjax();
     }
}

function lift_doAjaxCycle() {
  var queue = lift_ajaxQueue;
  if (queue.length > 0) {
    var now = (new Date()).getTime();
    if (lift_ajaxInProcess == null && queue[0].when <= now) {
      var aboutToSend = queue.shift();

      lift_ajaxInProcess = aboutToSend;
      var  successFunc = function() {
         lift_ajaxInProcess = null;
         if (aboutToSend.onSuccess) {
           aboutToSend.onSuccess();
         }
         lift_doAjaxCycle();
      };

      var failureFunc = function() {
         lift_ajaxInProcess = null;
         var cnt = aboutToSend.retryCnt;
         if (cnt < lift_ajaxRetryCount) {
	   aboutToSend.retryCnt = cnt + 1;
           var now = (new Date()).getTime();
           aboutToSend.when = now + (1000 * Math.pow(2, cnt));
           queue.push(aboutToSend);
           lift_ajaxQueueSort();
         } else {
           if (aboutToSend.onFailure) {
             aboutToSend.onFailure();
           } else {
             lift_defaultFailure();
           }
         }
         lift_doAjaxCycle();
      };
      lift_actualAjaxCall(aboutToSend.theData, successFunc, failureFunc);
    }
  }

  lift_testAndShowAjax();
  setTimeout("lift_doAjaxCycle();", 200);
}

function lift_actualAjaxCall(data, onSuccess, onFailure) {
"""+
                        LiftRules.jsArtifacts.ajax(AjaxInfo(JE.JsRaw("data"),
                                                            "POST", 5000, false, "script",
                                                            Full("onSuccess"), Full("onFailure")))+
                        """
}

"""+
                        LiftRules.jsArtifacts.onLoad(new JsCmd() {def toJsCmd = "lift_doAjaxCycle()"}).toJsCmd)

  var renderCometScript: LiftSession => JsCmd =
  session => JsCmds.Run("""
      function lift_handlerSuccessFunc() {setTimeout("lift_cometEntry();",100);}
      function lift_handlerFailureFunc() {setTimeout("lift_cometEntry();",10000);}
      function lift_cometEntry() {""" +
                        LiftRules.jsArtifacts.comet(AjaxInfo(JE.JsRaw("lift_toWatch"),
                                                             "GET",
                                                             140000,
                                                             false,
                                                             "script",
                                                             Full("lift_handlerSuccessFunc"),
                                                             Full("lift_handlerFailureFunc"))) + " } \n" +
                        LiftRules.jsArtifacts.onLoad(new JsCmd() {
        def toJsCmd = "lift_handlerSuccessFunc()"
      }).toJsCmd)

  var renderCometPageContents: (LiftSession, Seq[CometVersionPair]) => JsCmd =
  (session, vp) => JsCmds.Run(
    "var lift_toWatch = "+vp.map(p => p.guid.encJs+": "+p.version).mkString("{", " , ", "}")+";"
  )


  var ajaxScriptUpdateTime: LiftSession => Long = session => {
    object when extends SessionVar[Long](millis)
    when.is
  }

  var cometScriptUpdateTime: LiftSession => Long = session => {
    object when extends SessionVar[Long](millis)
    when.is
  }

  var ajaxScriptURL: String => String = url => url

  var cometScriptURL: String => String = url => url

  var ajaxScriptName: () => String = () => "liftAjax.js"

  var cometScriptName: () => String = () => "cometAjax.js"

  var serveCometScript: (LiftSession, Req) => Can[LiftResponse] =
  (liftSession, requestState) => {
    val modTime = cometScriptUpdateTime(liftSession)

    testFor304(requestState, modTime) or
    Full(JavaScriptResponse(renderCometScript(liftSession),
                            List("Last-Modified" -> toInternetDate(modTime)),
                            Nil, 200))
  }

  var serveAjaxScript: (LiftSession, Req) => Can[LiftResponse] =
  (liftSession, requestState) => {
    val modTime = ajaxScriptUpdateTime(liftSession)

    testFor304(requestState, modTime) or
    Full(JavaScriptResponse(renderAjaxScript(liftSession),
                            List("Last-Modified" -> toInternetDate(modTime)),
                            Nil, 200))
  }

  def testFor304(req: Req, lastModified: Long): Can[LiftResponse] = {
    val mod = req.request.getHeader("if-modified-since")
    if (mod != null && ((lastModified / 1000L) * 1000L) <= parseInternetDate(mod).getTime)
    Full(InMemoryResponse(new Array[Byte](0), Nil, Nil, 304))
    else
    Empty
  }
}

case object BreakOut

abstract class Bootable {
  def boot() : Unit;
}

private[http] case object DefaultBootstrap extends Bootable {
  def boot() : Unit = {
    val f = createInvoker("boot", Class.forName("bootstrap.liftweb.Boot").newInstance.asInstanceOf[AnyRef])
    f.map{f => f()}
  }
}

trait CometVersionPair {
  def guid: String
  def version: Long
}

case class CVP(guid: String, version: Long) extends CometVersionPair

case class AjaxMessageMeta(title: Can[String], cssClass: Can[String])
