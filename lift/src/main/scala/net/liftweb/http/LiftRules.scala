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

import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.sitemap._
import scala.xml._
import scala.collection.mutable.{ListBuffer}
import java.util.{Locale, TimeZone}
import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession, Cookie}
import javax.servlet.{ServletContext}
import java.io.InputStream
import java.io.ByteArrayOutputStream

object LiftRules {
  val noticesContainerId = "lift__noticesContainer__"
  
  type DispatchPf = PartialFunction[RequestMatcher, RequestState => Can[ResponseIt]];
  type RewritePf = PartialFunction[RewriteRequest, RewriteResponse]
  type TemplatePf = PartialFunction[RequestMatcher,() => Can[NodeSeq]]
  type SnippetPf = PartialFunction[List[String], NodeSeq => NodeSeq]
  type LiftTagPF = PartialFunction[(String, Elem, MetaData, NodeSeq, String), NodeSeq]
  type URINotFoundPF = PartialFunction[(RequestMatcher, Can[Failure]), ResponseIt]
  
  /**
  * A partial function that allows the application to define requests that should be
  * handled by lift rather than the default servlet handler
  */
  type LiftRequestPf = PartialFunction[RequestState, Boolean]
  
  private var _early: List[(HttpServletRequest) => Any] = Nil
  private[http] var _beforeSend: List[(Response, HttpServletResponse, List[(String, String)], Can[RequestState]) => Any] = Nil
  
  def appendBeforeSend(f: (Response, HttpServletResponse, List[(String, String)], Can[RequestState]) => Any) {
    _beforeSend = _beforeSend ::: List(f)
  }
  
  /**
  * The path to handle served resources
  */
  var ResourceServerPath = "classpath"
  
  private[http] var _afterSend: List[(Response, HttpServletResponse, List[(String, String)], Can[RequestState]) => Any] = Nil
  
  def appendAfterSend(f: (Response, HttpServletResponse, List[(String, String)], Can[RequestState]) => Any) {
    _afterSend = _afterSend ::: List(f)
  }
  
  /**
  * Determine the proper Content-Type based on the browser's Accept HTTP Header.
  */
  def determineContentType(req: Can[RequestState]) : String = {
    req match {
      case Full(request) => determineContentType(request.request)
      case _ => "text/html"
    }
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
  
  def determineContentType(request: HttpServletRequest) : String = {
    request match {
      case null => "text/html"
      case request => determineContentType(request.getHeader("Accept"))
    }
  }
  
  private[http] def notFoundOrIgnore(requestState: RequestState, session: Can[LiftSession]): Can[Response] = {
    if (passNotFoundToChain) Empty
    else session match {
      case Full(session) => Full(session.checkRedirect(requestState.createNotFound).toResponse)
      case _ => Full(requestState.createNotFound.toResponse)
    }
  }
  
  var liftTagProcessing: LiftTagPF = Map.empty
  
  /**
  * If you don't want lift to send the application/xhtml+xml mime type to those browsers
  * that understand it, then set this to {@code false}
  */
  var useXhtmlMimeType: Boolean = true
  
  def determineContentType(accept: String) : String = {
    // If application/xhtml+xml is explicitly listed then let's use that.
    if (useXhtmlMimeType && accept != null && accept.contains("application/xhtml+xml")) {
      "application/xhtml+xml"
    } else {
      "text/html"
    }
  }
  
  
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
  var calcRequestTimeout: Can[RequestState => Int] = Empty
  
  /**
  * If you want the standard (non-AJAX) request timeout to be something other than
  * 10 seconds, put the value here
  */
  var stdRequestTimeout: Can[Int] = Empty
  
  /**
  * If you want the AJAX request timeout to be something other than 120 seconds, put the value here
  */
  var ajaxRequestTimeout: Can[Int] = Empty
  
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
  * If the request times out (or returns a non-Response) you can
  * intercept the response here and create your own response
  */
  var requestTimedOut: Can[(RequestState, Any) => Can[Response]] = Empty
  
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
  * A function that takes the current HTTP request and returns the current
  */
  var localeCalculator: Can[HttpServletRequest] => Locale = defaultLocaleCalculator _
  
  def defaultLocaleCalculator(request: Can[HttpServletRequest]) = request.flatMap(_.getLocale() match {case null => Empty case l: Locale => Full(l)}).openOr(Locale.getDefault())
  
  
  val (hasJetty_?, contSupport, getContinuation, getObject, setObject, suspend, resume) = {
    try {
      val cc = Class.forName("org.mortbay.util.ajax.ContinuationSupport")
      val meth = cc.getMethod("getContinuation", Array(classOf[HttpServletRequest], classOf[AnyRef]))
      val cci = Class.forName("org.mortbay.util.ajax.Continuation")
      val getObj = cci.getMethod("getObject", null)
      val setObj = cci.getMethod("setObject", Array(classOf[AnyRef]))
      val suspend = cci.getMethod("suspend", Array(java.lang.Long.TYPE))
      val resume = cci.getMethod("resume", null)
      (true, (cc), (meth), (getObj), (setObj), (suspend), resume)
    } catch {
      case e => (false, null, null, null, null, null, null)
    }
  }
  
  def resumeRequest(what: AnyRef, req: HttpServletRequest) {
    val cont = getContinuation.invoke(contSupport, Array(req, LiftRules))
    setObject.invoke(cont, Array(what))
    resume.invoke(cont, null)
  }
  
  def doContinuation(req: HttpServletRequest, timeout: Long): Nothing = {
    try {
      val cont = getContinuation.invoke(contSupport, Array(req, LiftRules))
      Log.trace("About to suspend continuation")
      suspend.invoke(cont, Array(new java.lang.Long(timeout)))
      throw new Exception("Bail")
    } catch {
      case e: java.lang.reflect.InvocationTargetException if e.getCause.getClass.getName.endsWith("RetryRequest") =>
      throw e.getCause
    }
  }
  
  def checkJetty(req: HttpServletRequest): Option[Any] = {
    if (!hasJetty_?) None
    else {
      val cont = getContinuation.invoke(contSupport, Array(req, LiftRules))
      val ret = getObject.invoke(cont, null)
      setObject.invoke(cont, Array(null))
      Some(ret)
    }
  }
  
  private var _sitemap: Can[SiteMap] = Empty
  
  def setSiteMap(sm: SiteMap) {_sitemap = Full(sm)}
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
      case _ => SessionMaster.getSession(req) match {
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
      case _ => SessionMaster.getSession(req) match {
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
      case _ => SessionMaster.getSession(req) match {
        case Full(s) => S.initIfUninitted(s) {
          rpf(S.sessionTemplater.map(_.template), templateTable_i)
        }
        case _ => templateTable_i
      }
    }
  }
  
  var ajaxPath = "ajax_request"
  
  var cometPath = "comet_request"
  
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
  private def resourceFinder(name: String): java.net.URL = _context.getResource(name)
  
  def getResource(name: String): Can[java.net.URL] = resourceFinder(name) match {case null => defaultFinder(name) match {case null => Empty; case s => Full(s)} ; case s => Full(s)}
  def getResourceAsStream(name: String): Can[java.io.InputStream] = getResource(name).map(_.openStream)
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
  def loadResourceAsXml(name: String): Can[NodeSeq] = loadResourceAsString(name).flatMap(s =>PCDataXmlParser(s))
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
  
  /**
  * Takes a Node, headers, cookies, and a session and turns it into an XhtmlResponse.
  */
  private def cvt(ns: Node, headers: List[(String, String)], cookies: List[Cookie], session: RequestState) =
  convertResponse((XhtmlResponse(Group(session.fixHtml(ns)),
  ResponseInfo.docType(session),
  headers, cookies, 200), headers, cookies, session))
  
  var defaultHeaders: PartialFunction[(NodeSeq, RequestState), List[(String, String)]] = {
    case _ => List(("Expires", "0"))
  }
  
  /**
  * convertResponse is a PartialFunction that reduces a given Tuple4 into a
  * ResponseIt that can then be sent to the browser.
  */
  var convertResponse: PartialFunction[(Any, List[(String, String)], List[Cookie], RequestState), Response] = {
    case (r: ResponseIt, _, _, _) => r.toResponse
    case (ns: Group, headers, cookies, session) => cvt(ns, headers, cookies, session)
    case (ns: Node, headers, cookies, session) => cvt(ns, headers, cookies, session)
    case (ns: NodeSeq, headers, cookies, session) => cvt(Group(ns), headers, cookies, session)
    case (ns: Seq[Node], headers, cookies, session) => cvt(Group(ns), headers, cookies, session)
    
    case (Full(o), headers, cookies, session) => convertResponse( (o, headers, cookies, session) )
    
    case (Some(o), headers, cookies, session) => convertResponse( (o, headers, cookies, session) )
    case (bad, _, _, session) => session.createNotFound.toResponse
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
  * The function takes the RequestState and the Exception and returns a ResponseIt that's
  * sent to the browser.
  */
  var logAndReturnExceptionToBrowser: (RequestState, Throwable) => ResponseIt = showException
  
  /**
  * The partial function (pattern matching) for handling converting an exception to something to
  * be sent to the browser depending on the current RunMode (development, etc.)
  *
  * The best thing to do is browserResponseToException = { case (...) => } orElse browserResponseToException
  * so that your response over-rides the default, but the processing falls through to the default.
  */
  var browserResponseToException: PartialFunction[(Props.RunModes.Value, RequestState, Throwable), ResponseIt] = {
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
    case (RequestMatcher(r, _), _) => RequestState.defaultCreateNotFound(r)
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
  
  private def showException(r: RequestState, e: Throwable): ResponseIt = {
    Log.error("Exception being returned to browser when processing "+r, e)
    browserResponseToException(Props.mode, r, e)
  }
  
  var onBeginServicing: List[RequestState => Unit] = Nil
  var onEndServicing: List[(RequestState, Can[ResponseIt]) => Unit] = Nil
}

case object BreakOut

abstract class Bootable
{
  def boot() : Unit;
}

private[http] case object DefaultBootstrap extends Bootable
{
  def boot() : Unit =
  {
    val f = createInvoker("boot", Class.forName("bootstrap.liftweb.Boot").newInstance.asInstanceOf[AnyRef])
    f.map{f => f()}
  }
}

case class AjaxMessageMeta(title: Can[String], cssClass: Can[String])
