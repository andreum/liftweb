package net.liftweb.http

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
 
 
import javax.servlet.http._
import javax.servlet.ServletContext
// import scala.collection.Map
// import scala.collection.mutable.HashMap
import net.liftweb.util.Helpers._
import net.liftweb.util.{Log, Can, Full, Empty, Failure, ThreadGlobal}
import net.liftweb.sitemap._
import java.io.InputStream
import scala.xml._
import org.apache.commons.fileupload.servlet._

@serializable
sealed abstract class ParamHolder {
  def name: String
}
@serializable
case class NormalParamHolder(name: String, value: String) extends ParamHolder
@serializable
case class FileParamHolder(name: String, mimeType: String, fileName: String, file: Array[Byte]) extends ParamHolder

object RequestState {
  object NilPath extends ParsePath(Nil, true, false)
  
  def apply(request: HttpServletRequest, rewrite: LiftRules.RewritePf, nanoStart: Long): RequestState = {
    val reqType = RequestType(request)
    val turi = request.getRequestURI.substring(request.getContextPath.length)
    val tmpUri = if (turi.length > 0) turi else "/"
    val contextPath = request.getContextPath
    val tmpPath = parsePath(tmpUri)
    
    def processRewrite(path: ParsePath, params: Map[String, String]): RewriteResponse = {
      val toMatch = RewriteRequest(path, reqType, request)
      if (!rewrite.isDefinedAt(toMatch)) RewriteResponse(path, params)
      else {
        val resp = rewrite(toMatch)
        processRewrite(resp.path, resp.params)
        // rewrite(toMatch)
      }
    }
    
    
    // val (uri, path, localSingleParams) = processRewrite(tmpUri, tmpPath, TreeMap.empty)
    val rewritten = processRewrite(tmpPath, Map.empty)
    
    val localParams: Map[String, List[String]] = Map(rewritten.params.toList.map{case (name, value) => name -> List(value)} :_*)
    
    // val session = request.getSession
    //  val body = ()
    val eMap = Map.empty[String, List[String]]
    
    //    val (paramNames: List[String], params: Map[String, List[String]], files: List[FileParamHolder], body: Can[Array[Byte]]) =
    val paramCalculator = () =>
    if (reqType.post_? && request.getContentType == "text/xml") {
      (Nil,localParams, Nil, Full(readWholeStream(request.getInputStream)))
    } else if (ServletFileUpload.isMultipartContent(request)) {
      val allInfo = (new Iterator[ParamHolder] {
        val mimeUpload = (new ServletFileUpload)
        mimeUpload.setSizeMax(LiftRules.maxMimeSize)
        mimeUpload.setFileSizeMax(LiftRules.maxMimeFileSize)
        val what = mimeUpload.getItemIterator(request)
        def hasNext = what.hasNext
        def next = what.next match {
          case f if (f.isFormField) => NormalParamHolder(f.getFieldName, new String(readWholeStream(f.openStream), "UTF-8"))
          case f => FileParamHolder(f.getFieldName, f.getContentType, f.getName, readWholeStream(f.openStream)) 
        }
      }).toList
      
      val normal: List[NormalParamHolder] = allInfo.flatMap{case v: NormalParamHolder => List(v) case _ => Nil}
      val files: List[FileParamHolder] = allInfo.flatMap{case v: FileParamHolder => List(v) case _ => Nil}
      
      val params = normal.foldLeft(eMap)((a,b) => a.get(b.name) match {
        case None => a + (b.name -> List(b.value))
        case Some(v) => a + (b.name -> (v ::: List(b.value)))
      })
      
      (normal.map(_.name).removeDuplicates, localParams ++ params, files, Empty)
    } else if (reqType.get_?) {
      request.getQueryString match {
        case null => (Nil, localParams, Nil, Empty)
        case s =>
        val pairs = s.split("&").toList.map(_.trim).filter(_.length > 0).map(_.split("=").toList match {
          case name :: value :: Nil => (true, urlDecode(name), urlDecode(value))
          case name :: Nil => (true, urlDecode(name), "")
          case _ => (false, "", "")
          }).filter(_._1).map{case (_, name, value) => (name, value)}
          val names = pairs.map(_._1).removeDuplicates
          val params = pairs.foldLeft(eMap) (
          (a,b) => a.get(b._1) match {
            case None => a + (b._1 -> List(b._2))
            case Some(xs) => a + (b._1 -> (xs ::: List(b._2)))
          }
          )
          
          val hereParams = localParams ++ params
          
          (names, hereParams, Nil, Empty)
      }
    } else {
      val paramNames =  enumToStringList(request.getParameterNames).sort{(s1, s2) => s1 < s2}
      // val tmp = paramNames.map{n => (n, xlateIfGet(request.getParameterValues(n).toList))}
      val params = localParams ++ paramNames.map{n => (n, request.getParameterValues(n).toList)}
      (paramNames, params, Nil, Empty)
    }
    
    new RequestState(rewritten.path,contextPath, reqType, 
                     request.getContentType, request, nanoStart, System.nanoTime, paramCalculator)
  }
  
  private def fixURI(uri : String) = uri indexOf ";jsessionid"  match {
    case -1 => uri
    case x @ _ => uri substring(0, x)
  }
  
  def nil = new RequestState(NilPath, "", GetRequest, "", null, System.nanoTime, System.nanoTime,
                             () => (Nil, Map.empty, Nil, Empty))
  
  def parsePath(in: String): ParsePath = {
    val p1 = fixURI((in match {case null => "/"; case s if s.length == 0 => "/"; case s => s}).replaceAll("/+", "/"))
    val front = p1.startsWith("/")
    val back = p1.length > 1 && p1.endsWith("/")
    ParsePath(p1.replaceAll("/$", "/index").split("/").toList.filter(_.length > 0), front, back)
  }
  
  var fixHref = _fixHref _
  
  private def _fixHref(contextPath: String, v : Seq[Node], fixURL: Boolean): Text = {
    val hv = v.text
    if (hv.startsWith("/")) {
      Text(fixURL match {
        case true => URLRewriter.rewriteFunc map (_(contextPath+hv)) openOr contextPath+hv
        case _ => contextPath+hv
      })
    }
    else Text(hv)
  }
  
  def fixHtml(contextPath: String, in : NodeSeq) : NodeSeq = {
    if (contextPath.length == 0) in
    else {
      def fixAttrs(toFix : String, attrs : MetaData, fixURL: Boolean) : MetaData = {
        if (attrs == Null) Null 
        else if (attrs.key == toFix) {
          new UnprefixedAttribute(toFix, RequestState.fixHref(contextPath, attrs.value, fixURL),fixAttrs(toFix, attrs.next, fixURL))
        } else attrs.copy(fixAttrs(toFix, attrs.next, fixURL))
      }
      
      in.map{
        v => 
        v match {
          case Group(nodes) => Group(fixHtml(contextPath, nodes))
          
          case (<form>{ _* }</form>) => Elem(v.prefix, v.label, fixAttrs("action", v.attributes, true), v.scope, fixHtml(contextPath, v.child) : _* )
          case (<script>{ _* }</script>) => Elem(v.prefix, v.label, fixAttrs("src", v.attributes, false), v.scope, fixHtml(contextPath, v.child) : _* )
          case (<a>{ _* }</a>) => Elem(v.prefix, v.label, fixAttrs("href", v.attributes, true), v.scope, fixHtml(contextPath, v.child) : _* )
          case (<link/>) => Elem(v.prefix, v.label, fixAttrs("href", v.attributes, false), v.scope, fixHtml(contextPath, v.child) : _* )
          case Elem(_,_,_,_,_*) => Elem(v.prefix, v.label, fixAttrs("src", v.attributes, true), v.scope, fixHtml(contextPath, v.child) : _*)
          case _ => v
        }
      }
    }
  }
  
  private[liftweb] def defaultCreateNotFound(in: RequestState) = 
    XhtmlResponse((<html><body>The Requested URL {in.contextPath+in.uri} was not found on this server</body></html>),
      ResponseInfo.docType(in), List("Content-Type" -> "text/html"), Nil, 404)
  
  def unapply(in: RequestState): Option[(List[String], RequestType)] = Some((in.path.path, in.requestType)) 
}

@serializable
class RequestState(val path: ParsePath,
val contextPath: String,
val requestType: RequestType,
val contentType: String,
val request: HttpServletRequest,
val nanoStart: Long,
val nanoEnd: Long,
val paramCalculator: () => (List[String], Map[String, List[String]],List[FileParamHolder],Can[Array[Byte]])) 
{
  
  override def toString = "RequestState("+paramNames+", "+params+", "+path+
  ", "+contextPath+", "+requestType+", "+contentType+")"
  
  def xml_? = contentType != null && contentType.toLowerCase.startsWith("text/xml")
  val section = path(0) match {case null => "default"; case s => s}
  val view = path(1) match {case null => "index"; case s @ _ => s}
  val id = pathParam(0)
  def pathParam(n: Int) = head(path.path.drop(n + 2), "")
  def path(n: Int):String = head(path.path.drop(n), null)
  def param(n: String) = params.get(n) match {
    case Some(s :: _) => Some(s)
    case _ => None
  }
  
  lazy val (paramNames: List[String], params: Map[String, List[String]], uploadedFiles: List[FileParamHolder], body: Can[Array[Byte]]) =
  paramCalculator()
  
  lazy val cookies = request.getCookies() match {
    case null => Nil
    case ca => ca.toList
  }
  
  lazy val xml: Can[Elem] = if (!xml_?) Empty
  else {
    try {
      body.map(b => XML.load(new java.io.ByteArrayInputStream(b)))
    } catch {
      case e => Failure(e.getMessage, Full(e), Nil)
    }
  }
  
  lazy val location = LiftRules.siteMap.flatMap(_.findLoc(this))
  
  def testLocation: (Boolean, Can[ResponseIt]) = {
    if (LiftRules.siteMap.isEmpty) (true, Empty) 
    else location.map(_.testAccess) match {
      case Full((true, _)) => (true, Empty)
      case Full((_, Full(resp))) => (false, Full(resp))
      case _ => (false, Empty)
    }
  }
  
  
  lazy val buildMenu: CompleteMenu = location.map(_.buildMenu) openOr CompleteMenu(Nil)
  
  
  def createNotFound = {
    LiftRules.uriNotFound((RequestMatcher(this, S.session), Empty))
  }
  
  def createNotFound(failure: Failure) = { 
    LiftRules.uriNotFound((RequestMatcher(this, S.session), Can(failure)))
  }
  
  
  def post_? = requestType.post_?
  def get_? = requestType.get_?
  def put_? = requestType.put_?
  
  def fixHtml(in: NodeSeq): NodeSeq = RequestState.fixHtml(contextPath, in)
  
  lazy val uri = request.getRequestURI.substring(request.getContextPath.length) match {
    case "" => "/"
    case x => RequestState.fixURI(x)
  }
  
  /**
    * The IP address of the request
    */
  def remoteAddr: String = request.getRemoteAddr()
  
  /**
  * The user agent of the browser that sent the request
    */
  def userAgent: Can[String] = Can.legacyNullTest(request.getHeader("User-Agent"))
  
  def updateWithContextPath(uri: String): String = if (uri.startsWith("/")) contextPath + uri else uri
}


case class RequestMatcher(request: RequestState, session: Can[LiftSession])
case class RewriteRequest(path: ParsePath,requestType: RequestType, httpRequest: HttpServletRequest)
case class RewriteResponse(path: ParsePath, params: Map[String, String])


@serializable
case class ParsePath(path: List[String], absolute: Boolean, endSlash: Boolean) {
  def drop(cnt: Int) = ParsePath(path.drop(cnt), absolute, endSlash)
}

/**
 * Maintains the context of resolving the URL when cookies are disabled from container. It maintains
 * low coupling such as code within request processing is not aware of the servlet response that
 * ancodes the URL.
 */ 
object RewriteResponse {
  def apply(path: List[String], params: Map[String, String]) = new RewriteResponse(ParsePath(path, true, false), params)
  def apply(path: List[String]) = new RewriteResponse(ParsePath(path, true, false), Map.empty)
}

object URLRewriter {
  private val funcHolder = new ThreadGlobal[(String) => String]
  
  def doWith[R](f: (String) => String)(block : => R):R = {
    funcHolder.doWith(f) {
      block
    }
  }
  
  def rewriteFunc: Can[(String) => String] = Can.legacyNullTest(funcHolder value)
}
