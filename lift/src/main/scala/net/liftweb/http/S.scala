package net.liftweb.http

/*                                                
* (c) 2006-2008 WorldWide Conferencing, LLC
* Distributed under an Apache License
* http://www.apache.org/licenses/LICENSE-2.0
*/

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession, Cookie}
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.xml.{NodeSeq, Elem, Text, UnprefixedAttribute, Null, MetaData, Group, Node, HasKeyValue}
import scala.collection.immutable.{ListMap, TreeMap}
import net.liftweb.util.{Helpers, ThreadGlobal, LoanWrapper, Can, Empty, Full, Failure, Log, JSONParser}
import Helpers._
import js._
import java.io.InputStream
import java.util.{Locale, TimeZone, ResourceBundle}

/**
 * An object representing the current state of the HTTP request and response
 * It uses the DynamicVariable construct such that each thread has its own
 * local session info without passing a huge state construct around
 */
object S {
  /**
   * Holds the partial function that re-write an incoming request
   */
  case class RewriteHolder(name: String, rewrite: LiftRules.RewritePf)
  case class DispatchHolder(name: String, dispatch: LiftRules.DispatchPf)
  case class TemplateHolder(name: String, template: LiftRules.TemplatePf)
  
  case class CookieHolder(inCookies: List[Cookie], outCookies: List[Cookie]) {
    def add(in: Cookie) = CookieHolder(inCookies, in :: outCookies.filter(_.getName != in.getName))
    def delete(name: String) = {
      val c = new Cookie(name, "")
      c.setMaxAge(0)
      add(c)
    }
    def delete(old: Cookie) = {
      val c = old.clone().asInstanceOf[Cookie]
      c.setMaxAge(0)
      c.setValue("")
      add(c)
    }
  }
  
  /**
  * The current session
  */
  private val _request = new ThreadGlobal[RequestState]
  private val _functionMap = new ThreadGlobal[HashMap[String, AFuncHolder]]
  private val _notice = new ThreadGlobal[ListBuffer[(NoticeType.Value, NodeSeq, Can[String])]]
  private val _oldNotice = new ThreadGlobal[Seq[(NoticeType.Value, NodeSeq, Can[String])]];
  private val inS = (new ThreadGlobal[Boolean]).set(false)
  private val snippetMap = new ThreadGlobal[HashMap[String, NodeSeq => NodeSeq]]
  private val _attrs = new ThreadGlobal[Map[String, String]]
  private val _requestVar = new ThreadGlobal[HashMap[String, Any]]
  private val _sessionInfo = new ThreadGlobal[LiftSession]
  private val _queryLog = new ThreadGlobal[ListBuffer[(String, Long)]]
  private val _resBundle = new ThreadGlobal[Can[ResourceBundle]]
  private val _liftCoreResBundle = new ThreadGlobal[Can[ResourceBundle]]
  private val _stateSnip = new ThreadGlobal[HashMap[String, StatefulSnippet]]
  private val _responseHeaders = new ThreadGlobal[ResponseInfoHolder]
  private val _responseCookies = new ThreadGlobal[CookieHolder]
  
  /**
  * Get the current RequestState
  *
  * @return the current RequestState
  */
  def request: Can[RequestState] = _request.value match {case null => Empty case r => Full(r)}
  
  /**
   * @return a List of any Cookies that have been set for this Response.
   */
  def receivedCookies: List[Cookie] =
    for (rc <- Can.legacyNullTest(_responseCookies.value).toList; c <- rc.inCookies) 
      yield c.clone().asInstanceOf[Cookie]
  
  /**
   * Finds a cookie with the given name
   * @param name - the name of the cookie to find
   *
   * @return a Can of the cookie
   */
  def findCookie(name: String): Can[Cookie] = 
  Can.legacyNullTest(_responseCookies.value).flatMap(
    rc => Can(rc.inCookies.filter(_.getName == name)).
              map(_.clone().asInstanceOf[Cookie]))
  
  def responseCookies: List[Cookie] = Can.legacyNullTest(_responseCookies.value).
    toList.flatMap(_.outCookies)
  
  /**
   * Adds a Cookie to the List[Cookies] that will be sent with the Response.
   *
   * If you wish to delete a Cookie as part of the Response, add a Cookie with
   * a MaxAge of 0.
   */
  def addCookie(cookie: Cookie) {
    Can.legacyNullTest(_responseCookies.value).foreach(rc =>
      _responseCookies.set(rc.add(cookie))
    )
  }
  
  /**
   * Deletes the cookie from the user's browser.
   * @param cookie the Cookie to delete
   */
  def deleteCookie(cookie: Cookie) {
    Can.legacyNullTest(_responseCookies.value).foreach(rc => 
      _responseCookies.set(rc.delete(cookie))
    )
  }
  
  /**
   * Deletes the cookie from the user's browser.
   * @param name the name of the cookie to delete
   */
  def deleteCookie(name: String) {
    Can.legacyNullTest(_responseCookies.value).foreach(rc => 
      _responseCookies.set(rc.delete(name))
    )
  }
  
  
  /**
  * Returns the Locale for this request based on the HTTP request's 
  * Accept-Language header. If that header corresponds to a Locale
  * that's installed on this JVM then return it, otherwise return the
  * default Locale for this JVM.
  */
  def locale: Locale = LiftRules.localeCalculator(request.map(_.request))
  
  /**
   * Return the current timezone
   */
  def timeZone: TimeZone = 
  LiftRules.timeZoneCalculator(request.map(_.request))
  
  private def reduxio(in: List[LiftRules.DispatchPf]): LiftRules.DispatchPf = in match {
    case Nil => Map.empty
    case x :: Nil => x
    case x :: xs => x orElse reduxio(xs)
  }
  
  def highLevelSessionDispatcher: LiftRules.DispatchPf = reduxio(highLevelSessionDispatchList.map(_.dispatch))
  /**
   * Return the list of DispatchHolders set for this session. 
   */
  def highLevelSessionDispatchList: List[DispatchHolder] = 
    session map (_.highLevelSessionDispatcher.toList.map(t => DispatchHolder(t._1, t._2))) openOr Nil
  
  def addHighLevelSessionDispatcher(name: String, disp: LiftRules.DispatchPf) = 
    session map (_.highLevelSessionDispatcher += (name -> disp))
  
  def removeHighLevelSessionDispatcher(name: String) =
    session map (_.highLevelSessionDispatcher -= name)
  
  def clearHighLevelSessionDispatcher = session map (_.highLevelSessionDispatcher.clear)
  
  /**
   * Gets the list of templaters (partial functions that match and return a template rather than
   * loading a template from a file or a class)
   */
  def sessionTemplater: List[TemplateHolder] =
    session map (_.sessionTemplater.toList.map(t => TemplateHolder(t._1, t._2))) openOr Nil

  def addSessionTemplater(name: String, rw: LiftRules.TemplatePf) =
    session map (_.sessionTemplater += (name -> rw))

  def removeSessionTemplater(name: String) =
    session map (_.sessionTemplater -= name)

  def clearSessionTemplater = session map (_.sessionTemplater.clear)

  
  def sessionRewriter: List[RewriteHolder] =
    session map (_.sessionRewriter.toList.map(t => RewriteHolder(t._1, t._2))) openOr Nil
      
  def addSessionRewriter(name: String, rw: LiftRules.RewritePf) =
    session map (_.sessionRewriter += (name -> rw))
  
  def removeSessionRewriter(name: String) =
    session map (_.sessionRewriter -= name)
  
  def clearSessionRewriter = session map (_.sessionRewriter.clear)

  /**
   * Test the current request to see if it's a POST
   */
  def post_? = request.map(_.post_?).openOr(false);
  
  /**
   * Localize the incoming string based on a resource bundle for the current locale
   * @param str the string or ID to localize (the default value is the
   *
   * @return the localized XML or Empty if there's no way to do localization
   */
  def loc(str: String): Can[NodeSeq] =
  resourceBundle.flatMap(r => tryo(r.getObject(str) match {
    case null => LiftRules.localizationLookupFailureNotice.foreach(_(str, locale)); Empty
    case s: String => Full(LiftRules.localizeStringToXml(s))
    case g: Group => Full(g)
    case e: Elem => Full(e)
    case n: Node => Full(n)
    case ns: NodeSeq => Full(ns)
    case x => Full(Text(x.toString))
  }).flatMap(s => s))
  
  /**
  * Get the resource bundle for the current locale
  */
  def resourceBundle: Can[ResourceBundle] = Can.legacyNullTest(_resBundle.value).openOr {
    val rb = tryo(ResourceBundle.getBundle(LiftRules.resourceName, locale))
    _resBundle.set(rb)
    rb
  }
  
  /**
  * Get the lift core resource bundle for the current locale
  */
  def liftCoreResourceBundle: Can[ResourceBundle] = Can.legacyNullTest(_liftCoreResBundle.value).openOr {
    val rb = tryo(ResourceBundle.getBundle(LiftRules.liftCoreResourceName, locale))
    _liftCoreResBundle.set(rb)
    rb
  }
  
  /**
  * Get a localized string or return the original string
  *
  * @param str the string to localize
  *
  * @return the localized version of the string
  */
  def ?(str: String): String = resourceBundle.flatMap(r => tryo(r.getObject(str) match 
  {case s: String => Full(s) case _ => Empty}).flatMap(s => s)).
  openOr{LiftRules.localizationLookupFailureNotice.foreach(_(str, locale)); str}
  
  def ?(str: String, params: Any *): String = if (params.length == 0) 
  ?(str)
  else
  String.format(locale, ?(str), params.flatMap{case s: AnyRef => List(s) case _ => Nil}.toArray) 
  
  /**
  * Get a core lift localized string or return the original string
  *
  * @param str the string to localize
  *
  * @return the localized version of the string
  */
  def ??(str: String): String = liftCoreResourceBundle.flatMap(r => tryo(r.getObject(str) match 
  {case s: String => Full(s) case _ => Empty}).flatMap(s => s)).
  openOr{LiftRules.localizationLookupFailureNotice.foreach(_(str, locale)); str
  }
  
  /**
  * Localize the incoming string based on a resource bundle for the current locale
  * @param str the string or ID to localize
  * @param dflt the default string to return if localization fails
  *
  * @return the localized XHTML or default value
  */
  def loc(str: String, dflt: NodeSeq): NodeSeq = loc(str).openOr(dflt)
  
  
  /**
  * Test the current request to see if it's a GET
  */
  def get_? = request.map(_.get_?).openOr(false)
  
  /**
  * The URI of the current request (not re-written)
  */
  def uri: String = request.map(_.uri).openOr("/")
  
  /**
  * Redirect the browser to a given URL
  */
  def redirectTo[T](where: String): T = throw ResponseShortcutException.redirect(where) 
  
  def redirectTo[T](where: String, func: () => Unit): T = 
  throw ResponseShortcutException.redirect(where, func)
  
  private val executionInfo = new ThreadGlobal[HashMap[String, Function[Array[String], Any]]]
  
  private val currCnt = new ThreadGlobal[Int]
  
  private def initNotice[B](f: => B): B = {
    _notice.doWith(new ListBuffer[(NoticeType.Value, NodeSeq, Can[String])])(f)
  }
  
  /**
  * Initialize the current request session
  */
  def init[B](request: RequestState, session: LiftSession)(f: => B) : B = {
      _oldNotice.doWith(Nil) {
        _init(request,session)(() => f)
      }
  }
  
  /**
  * The current LiftSession
  */
  def session: Can[LiftSession] = Can.legacyNullTest(_sessionInfo.value)
  
  /**
  * Log a query for the given request.  The query log can be tested to see
  * if queries for the particular page rendering took too long
  */
  def logQuery(query: String, time: Long) = Can.legacyNullTest(_queryLog.value).foreach(_ += (query, time))
  
  private[http] def snippetForClass(cls: String): Can[StatefulSnippet] =
    Can.legacyNullTest(_stateSnip.value).flatMap(_.get(cls))
  
  private[http] def setSnippetForClass(cls: String, inst: StatefulSnippet): Unit = 
    Can.legacyNullTest(_stateSnip.value).foreach(_(cls) = inst)

  private[http] def unsetSnippetForClass(cls: String): Unit = 
    Can.legacyNullTest(_stateSnip.value).foreach(_ -= cls)
    
    
  private var _queryAnalyzer: List[(Can[RequestState], Long, List[(String, Long)]) => Any] = Nil
  
  /**
  * Add a query analyzer (passed queries for analysis or logging)
  */
  def addAnalyzer(f: (Can[RequestState], Long, List[(String, Long)]) => Any): Unit = _queryAnalyzer = _queryAnalyzer ::: List(f)
  
  private var aroundRequest: List[LoanWrapper] = Nil
  private def doAround[B](ar: List[LoanWrapper])(f: => B): B = 
  ar match {
    case Nil => f
    case x :: xs => x(doAround(xs)(f))
  }
  
  /**
  * You can wrap the handling of an HTTP request with your own wrapper.  The wrapper can
  * execute code before and after the request is processed (but still have S scope).
  * This allows for query analysis, etc.
  */  
  def addAround(lw: List[LoanWrapper]): Unit = aroundRequest = lw ::: aroundRequest 
  
  /**
  * Get a list of the logged queries
  */
  def queryLog: List[(String, Long)] = Can.legacyNullTest(_queryLog.value).map(_.toList).openOr(Nil)
  
  private def wrapQuery[B](f:() => B): B = {
    _queryLog.doWith(new ListBuffer) {
      val begin = millis
      try {
        f()
      } finally {
        val time = millis - begin
        _queryAnalyzer.foreach(_(request, time, queryLog))
      }
    }
  }
  
  def setHeader(name: String, value: String) {
    Can.legacyNullTest(_responseHeaders.value).foreach(
    rh =>
    rh.headers = rh.headers + (name -> value)
    )
  }
  
  def getHeaders(in: List[(String, String)]): List[(String, String)] = {
    Can.legacyNullTest(_responseHeaders.value).map(
    rh =>
    rh.headers.elements.toList ::: 
    in.filter{case (n, v) => !rh.headers.contains(n)}
    ).openOr(Nil)
  }
  
  def setDocType(what: Can[String]) {
    Can.legacyNullTest(_responseHeaders.value).foreach(
    rh =>
    rh.docType = what
    )
  }
  
  def getDocType: (Boolean, Can[String]) =
  Can.legacyNullTest(_responseHeaders.value).map(
  rh => (rh.overrodeDocType, rh.docType)
  ).openOr( (false, Empty) )
  
  private def _innerInit[B](f: () => B): B = {
    _attrs.doWith(new TreeMap) {
      snippetMap.doWith(new HashMap) {
        _resBundle.doWith(null) {
          _liftCoreResBundle.doWith(null){
            inS.doWith(true) {
              _stateSnip.doWith(new HashMap) {
                initNotice {
                  _functionMap.doWith(new HashMap[String, AFuncHolder]) {
		            doAround(aroundRequest) {
		              wrapQuery {
			            this.currCnt.doWith(0)(f)
		              }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  /**
   * @return a List[Cookie] even if the underlying request's Cookies are null.
   */
  private def getCookies(request: HttpServletRequest): List[Cookie] =
  for (r <- Can.legacyNullTest(request).toList;
  ca <- Can.legacyNullTest(r.getCookies).toList;
  c <- ca) yield c

  private def _init[B](request: RequestState, session: LiftSession)(f: () => B): B = {
    this._request.doWith(request) {
      _sessionInfo.doWith(session) {
        _responseHeaders.doWith(new ResponseInfoHolder) {
          _requestVar.doWith(new HashMap) {
	    _responseCookies.doWith(CookieHolder(getCookies(request.request), Nil)) {
              _innerInit(f)
	    }
          }
        }
      }
    }
  }
  
  def referer: Can[String] = request.flatMap(r => Can.legacyNullTest(r.request.getHeader("Referer")))
  
  private[http] object requestState {
    private def rv: Can[HashMap[String, Any]] = Can.legacyNullTest(_requestVar.value) //  match {case null => Empty case v => Full(v)}
    
    def apply[T](name: String): Can[T] = rv.flatMap(r => Can(r.get(name).asInstanceOf[Option[T]]))
    
    def update[T](name: String, value: T): Unit = rv.foreach(_(name) = value)
    
    def clear(name: String): Unit = rv.foreach(_ -= name) 
  }
  
  object attr {
    def apply(what: String): Can[String] = Can(S._attrs.value.get(what))
  }
  
  def setVars[T](attr: MetaData)(f: => T): T = {
    _attrs.doWith(_attrs.value ++ attr.toList.map(m => (m.key, m.value.text)))(f)
  }
  
  def initIfUninitted[B](session: LiftSession)(f: => B) : B = {
    if (inS.value) f
    else init(RequestState.nil,session)(f)
  }
  
  def init[B](request: RequestState, oldNotices: Seq[(NoticeType.Value, NodeSeq, Can[String])], session: LiftSession)(f : => B) : B = {
    _oldNotice.doWith(oldNotices) {
        _init(request, session)(() => f)
    }
  }

  def get(what: String): Can[String] = session.flatMap(_.get(what))

  def getSessionAttribute(what: String): Can[String] = servletSession.flatMap(_.getAttribute(what) match {case s: String => Full(s) case _ => Empty}) 
  
  
  def servletSession: Can[HttpSession] = session.map(_.httpSession).or(servletRequest.map(_.getSession))
    
  def invokedAs: String = _attrs.value.get("type") getOrElse ""
  
  def setSessionAttribute(name: String, value: String) = servletSession.foreach(_.setAttribute(name, value)) 

  def set(name: String, value: String) = session.foreach(_.set(name,value))
  
  
  def unsetSessionAttribute(name: String) = servletSession.foreach(_.removeAttribute(name))

  def unset(name: String) = session.foreach(_.unset(name))
  
  /**
  * The current servlet request
  */
  def servletRequest: Can[HttpServletRequest] = Can.legacyNullTest(_request.value).map(_.request)
  
  /**
  * The host that the request was made on
  */
  def hostName: String = servletRequest.map(_.getServerName).openOr("nowhere_123.com")
  
  /**
  * The host and path of the quest
  */
  def hostAndPath: String = servletRequest.map(r => r.getScheme + "://"+r.getServerName+":"+r.getServerPort+r.getContextPath).openOr("")
  
  /**
  * Get a map of the name/functions
  */
  def functionMap: Map[String, AFuncHolder] = Can.legacyNullTest(_functionMap.value).map(s => Map(s.elements.toList :_*)).openOr(Map.empty)
  
  /**
  * The current context path
  */
  def contextPath = session.map(_.contextPath).openOr("")  
  
  def locateSnippet(name: String): Can[NodeSeq => NodeSeq] = Can(snippetMap.value.get(name)) or {
    val snippet = if (name.indexOf(".") != -1) name.roboSplit("\\.") else name.roboSplit(":") // name.split(":").toList.map(_.trim).filter(_.length > 0)
    if (LiftRules.snippetTable.isDefinedAt(snippet)) Full(LiftRules.snippetTable(snippet)) else Empty
  }
  
  def mapSnippet(name: String, func: NodeSeq => NodeSeq) {snippetMap.value(name) = func}
  
  
  def addFunctionMap(name: String, value: AFuncHolder) = _functionMap.value += (name -> value)
  
  
  private def booster(lst: List[String], func: String => Any): Unit = lst.foreach(v => func(v))
  
  def encodeURL(url: String) = {
    URLRewriter.rewriteFunc map (f => f(url)) openOr url
  }
  
  
  /**
  * Build a handler for incoming JSON commands
  *
   * @param f - function returning a JsCmds
   * @return (JsonCall, JsCmd)
   */
  def buildJsonFunc(f: Any => JsCmd): (JsonCall, JsCmd) = buildJsonFunc(Empty, f)
  
  /**
  * Build a handler for incoming JSON commands
  *
  * @param name -- the optional name of the command (placed in a comment for testing)
  *
   * @param f - function returning a JsCmds
   * @return (JsonCall, JsCmd)
   */
  def buildJsonFunc(name: Can[String], f: Any => JsCmd): (JsonCall, JsCmd) = {
    val key = "F"+System.nanoTime+"_"+randomString(3)
      
    def checkCmd(in: Any) = in match {
      case v: scala.collection.Map[String, Any] if v.isDefinedAt("command") => 
      JsonCmd(v("command").toString, v.get("target").
        map {
          case null => null
          case x => x.toString
     } . getOrElse(null),v.get("params").getOrElse(None), v)
  
     case v => v
    }

    def jsonCallback(in: List[String]): JsCmd = {
      in.flatMap(
      s => 
        JSONParser.parse(s.trim).toList.map(checkCmd).map(f)
      ).foldLeft(JsCmds.Noop)(_ & _)
    }

    addFunctionMap(key, jsonCallback _)

    (JsonCall(key), JsCmds.Run(name.map(n => "/* JSON Func "+n+" $$ "+key+" */").openOr("") + 
    "function "+key+"(obj) {jQuery.ajax( {url: '"+encodeURL(contextPath+"/"+LiftRules.ajaxPath)+"', cache: false, timeout: 10000, type: 'POST', data: '"+
       key+"='+encodeURIComponent(JSON.stringify(obj)) , dataType: 'script'});}"))
  }
  
  /**
   * Returns the JsCmd that holds the notices markup
   *
   */
  private[http] def noticesToJsCmd: JsCmd = {
      
    val func: (() => List[NodeSeq], String, MetaData) => NodeSeq = (f, title, attr) => f() map (e => <li>{e}</li>) match {
      case Nil => Nil
      case list => <div>{title}<ul>{list}</ul></div> % attr
    }
  
    val f = noIdMessages _
    val xml = List((LiftRules.ajaxNoticeMeta, f(S.errors), S.??("msg.error")), 
                   (LiftRules.ajaxWarningMeta, f(S.warnings), S.??("msg.warning")), 
                   (LiftRules.ajaxErrorMeta, f(S.notices), S.??("msg.notice"))) flatMap {
     msg => msg._1 match {
         case Full(meta) => func(msg._2 _, meta.title openOr "", meta.cssClass.map(new UnprefixedAttribute("class", _, Null)) openOr Null)
         case _ => func(msg._2 _, msg._3, Null)
     }
    }
      
    val groupMessages = xml match {
      case Nil => JsCmds.Noop 
      case _ => JsCmds.SetHtml(LiftRules.noticesContainerId, xml)
    }
      
    val g = idMessages _
    List((LiftRules.ajaxErrorMeta, g(S.errors)), 
         (LiftRules.ajaxWarningMeta, g(S.warnings)), 
         (LiftRules.ajaxNoticeMeta, g(S.notices))).foldLeft(groupMessages)((car, cdr) => cdr match {
           case (meta, m) => m.foldLeft(car)((left, r) => 
             left & JsCmds.SetHtml(r._1, <span>{r._2 flatMap(node => node)}</span> % 
              (meta map(_.cssClass.map(new UnprefixedAttribute("class", _, Null)) openOr Null) openOr Null)))
         }
         )
  }
    
  implicit def toLFunc(in: List[String] => Any): AFuncHolder = LFuncHolder(in, Empty)
  implicit def toNFunc(in: () => Any): AFuncHolder = NFuncHolder(in, Empty)
  implicit def stuff2ToUnpref(in: (Symbol, Any)): UnprefixedAttribute = new UnprefixedAttribute(in._1.name, Text(in._2.toString), Null)
  
  def mapFuncToURI(uri: String, f : () => Unit): String = {
    session map (_ attachRedirectFunc(uri, Can.legacyNullTest(f))) openOr uri
  }
  
  @serializable
  abstract class AFuncHolder {
    def owner: Can[String]
    def apply(in: List[String]): Any
    def duplicate(newOwner: String): AFuncHolder
  }
    
  @serializable  
  class BinFuncHolder(val func: FileParamHolder => Any, val owner: Can[String]) extends AFuncHolder {
    def apply(in: List[String]) {Log.error("You attempted to call a 'File Upload' function with a normal parameter.  Did you forget to 'enctype' to 'multipart/form-data'?")}
    def apply(in: FileParamHolder) = func(in)
    def duplicate(newOwner: String) = new BinFuncHolder(func, Full(newOwner))
  }
    
  object BinFuncHolder {
    def apply(func: FileParamHolder => Any) = new BinFuncHolder(func, Empty)
    def apply(func: FileParamHolder => Any, owner: Can[String]) = new BinFuncHolder(func, owner)
  }
    
  object SFuncHolder {
    def apply(func: String => Any) = new SFuncHolder(func, Empty)
    def apply(func: String => Any, owner: Can[String]) = new SFuncHolder(func, owner)
  }
    
  @serializable
  class SFuncHolder(val func: String => Any,val owner: Can[String]) extends AFuncHolder {
    def this(func: String => Any) = this(func, Empty)
    def apply(in: List[String]): Any = in.map(func(_))
    def duplicate(newOwner: String) = new SFuncHolder(func, Full(newOwner))    
  }
    
  object LFuncHolder {
    def apply(func: List[String] => Any) = new LFuncHolder(func, Empty)
    def apply(func: List[String] => Any, owner: Can[String]) = new LFuncHolder(func, owner)
  }
    
  @serializable
  class LFuncHolder(val func: List[String] => Any,val owner: Can[String]) extends AFuncHolder {
    def apply(in: List[String]): Any = func(in)
    def duplicate(newOwner: String) = new LFuncHolder(func, Full(newOwner))    
  }
    
  object NFuncHolder {
    def apply(func: () => Any) = new NFuncHolder(func, Empty)
    def apply(func: () => Any, owner: Can[String]) = new NFuncHolder(func, owner)
  }
    
  @serializable
  class NFuncHolder(val func: () => Any,val owner: Can[String]) extends AFuncHolder {
    def apply(in: List[String]): Any = in.map(s => func())
    def duplicate(newOwner: String) = new NFuncHolder(func, Full(newOwner))        
  }
    
  def mapFunc(in: AFuncHolder): String = mapFunc("F"+System.nanoTime+"_"+randomString(3), in)
    
  def mapFunc(name: String, inf: AFuncHolder): String = {
    addFunctionMap(name, inf)
      name
  }
    
    
  def params(n: String): List[String] = request.map(_.params(n)).openOr(Nil)
  def param(n: String): Can[String] = request.flatMap(r => Can(r.param(n)))
    
  def error(n: String) {error(Text(n))}
  def error(n: NodeSeq) {_notice.value += (NoticeType.Error, n,  Empty)}
  def error(id:String, n: NodeSeq) {_notice.value += (NoticeType.Error, n,  Full(id))}
  def error(id:String, n: String) {error(id, Text(n))}
  def notice(n: String) {notice(Text(n))}
  def notice(n: NodeSeq) {_notice.value += (NoticeType.Notice, n, Empty)}
  def notice(id:String, n: NodeSeq) {_notice.value += (NoticeType.Notice, n,  Full(id))}
  def notice(id:String, n: String) {notice(id, Text(n))}
  def warning(n: String) {warning(Text(n))}
  def warning(n: NodeSeq) {_notice.value += (NoticeType.Warning, n, Empty)}
  def warning(id:String, n: NodeSeq) {_notice.value += (NoticeType.Warning, n,  Full(id))}
  def warning(id:String, n: String) {warning(id, Text(n))}

  def error(vi: List[FieldError]) {_notice.value ++= vi.map{i => (NoticeType.Error, i.msg, i.field.uniqueFieldId )}}
    
    
  private [http] def message(msg: String, notice: NoticeType.Value) { message(Text(msg), notice)}
  private [http] def message(msg: NodeSeq, notice: NoticeType.Value) { _notice.value += (notice, msg, Empty)}
  private [http] def messagesFromList(list: List[(NoticeType.Value, NodeSeq, Can[String])]) { list foreach ( _notice.value += _) } 
    
  def getNotices: List[(NoticeType.Value, NodeSeq, Can[String])] = 
    Can.legacyNullTest(_notice.value).toList.flatMap(_.toList)
    
  def errors: List[(NodeSeq, Can[String])] = List(_oldNotice.value, _notice.value).flatMap(_.filter(_._1 == NoticeType.Error).map(n => (n._2, n._3)))
  def notices: List[(NodeSeq, Can[String])] = List(_oldNotice.value, _notice.value).flatMap(_.filter(_._1 == NoticeType.Notice).map(n => (n._2, n._3)))
  def warnings: List[(NodeSeq, Can[String])] = List(_oldNotice.value, _notice.value).flatMap(_.filter(_._1 == NoticeType.Warning).map(n => (n._2, n._3)))
  def clearCurrentNotices {_notice.value.clear}
  
  /**
   * Returns the messages provided by list function that are associated with id
   *
   * @param id - the lookup id
   * @param f - the function that returns the messages 
   */
  def messagesById(id: String)(f: => List[(NodeSeq, Can[String])]): List[NodeSeq] = f filter( _._2 map (_ equals id ) openOr false) map(_._1)
    
  /**
   *  Returns the messages that are not associated with any id
   *
   * @param f - the function that returns the messages 
   */
  def noIdMessages(f: => List[(NodeSeq, Can[String])]): List[NodeSeq] = f filter( _._2 isEmpty) map (_._1)

  /**
   * Returns the messages that are associated with any id. 
   * Messages associated with the same id will be enlisted.
   *
   * @param f - the function that returns the messages 
   */
  def idMessages(f: => List[(NodeSeq, Can[String])]):List[(String, List[NodeSeq])] = {
    val res = new HashMap[String, List[NodeSeq]]
    f filter(  _._2.isEmpty == false) foreach (_ match {
      case (node, id) => val key = id open_!; res += key -> (res.getOrElseUpdate(key, Nil) ::: List(node))
    })
       
    res toList
  }

  implicit def tuple2FieldError(t: (FieldIdentifier, NodeSeq)) = FieldError(t._1, t._2)
    
}
    
  @serializable
  object NoticeType extends Enumeration {
    val Notice, Warning, Error = Value
  }
    
  abstract class JsonHandler {
    private val name = "_lift_json_"+getClass.getName
    private def handlers: (JsonCall, JsCmd) = 
      S.session.map(s => s.get[Any](name) match {
      case Full((x: JsonCall, y: JsCmd)) =>  (x, y) 
      
       case _ => 
         val ret: (JsonCall, JsCmd) = S.buildJsonFunc(this.apply)
         s.set(name, ret)
         ret
      }
      ).openOr( (JsonCall(""), JsCmds.Noop) )
    
    def call: JsonCall = handlers._1
      
    def jsCmd: JsCmd = handlers._2
      
    def apply(in: Any): JsCmd
  }
    
  abstract class AnyVar[T, MyType <: AnyVar[T, MyType]](dflt: => T) { self: MyType =>
    private val name = "_lift_sv_"+getClass.getName // "V"+randomString(10)
    protected def findFunc(name: String): Can[T]
    protected def setFunc(name: String, value: T): Unit
    protected def clearFunc(name: String): Unit
     
    /**
     * The current value of the session variable
     */
    def is: T = findFunc(name) match {
      case Full(v) => v
      case _ => val ret = dflt
        apply(ret)
        ret
    }
      
    /**
     * Set the session variable
     *
     * @param what -- the value to set the session variable to
     */
    def apply(what: T): Unit = setFunc(name, what)
      
    def remove(): Unit = clearFunc(name)   
      
    override def toString = is.toString
  }
    
  /**
   * Keep session information around without the nastiness of naming session variables
   * or the type-unsafety of casting the results.
   * SessionVars are type-safe variables that map pretty directly to  
   * HttpSession attributes.  Put stuff in and they are available for the  
   * life of the Session.
   *
   * @param dflt - the default value of the session variable
   */
  abstract class SessionVar[T](dflt: => T) extends AnyVar[T, SessionVar[T]](dflt) {
    override protected def findFunc(name: String): Can[T] = S.session.flatMap(_.get(name))
    override protected def setFunc(name: String, value: T): Unit = S.session.foreach(_.set(name, value))
    override protected def clearFunc(name: String): Unit = S.session.foreach(_.unset(name))
  }
    
  /**
   * Keep request-local information around without the nastiness of naming session variables
   * or the type-unsafety of casting the results.
   * RequestVars share their value through the scope of the current HTTP  
   * request.  They have no value at the beginning of request servicing  
   * and their value is discarded at the end of request processing.  They  
   * are helpful to share values across many snippets.
   *
   * @param dflt - the default value of the session variable
   */
  abstract class RequestVar[T](dflt: => T) extends AnyVar[T, RequestVar[T]](dflt) {
    override protected def findFunc(name: String): Can[T] = S.requestState(name) // S.servletSession.flatMap(_.getAttribute(name) match {case Full(v: T) => Full(v) case _ => Empty})
    override protected def setFunc(name: String, value: T): Unit = S.requestState(name) = value // S.servletSession.foreach(_.setAttribute(name, Full(value)))
    override protected def clearFunc(name: String): Unit = S.requestState.clear(name)
  }
    
    
    
  object AnyVar {
    implicit def whatSessionVarIs[T](in: SessionVar[T]): T = in.is
    implicit def whatRequestVarIs[T](in: RequestVar[T]): T = in.is
  }
    
  case class JsonCmd(command: String, target: String, params: Any,
    all: scala.collection.Map[String, Any])
    
  class ResponseInfoHolder {
    var headers: Map[String, String] = Map.empty
    private var _docType: Can[String] = Empty
    private var _setDocType = false
      
    def docType = _docType
    def docType_=(in: Can[String]) {
      _docType = in
      _setDocType = true
    }
      
    def overrodeDocType = _setDocType
  }
    
  /**
   * Defines the association of this reference with an markup tag ID
   */
  trait FieldIdentifier {
    def uniqueFieldId: Can[String] = Empty
  }
    
  /**
   * Associate a FieldIdentifier with an NodeSeq
   */
  case class FieldError(field : FieldIdentifier, msg : NodeSeq)
    
