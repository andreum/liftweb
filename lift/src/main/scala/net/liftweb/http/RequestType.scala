package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import javax.servlet.http.{HttpServletRequest}

@serializable
abstract class RequestType {
  def post_? : Boolean = false
  def get_? : Boolean = false
  def head_? : Boolean = false
  def put_? : Boolean = false
  def delete_? : Boolean = false
}

@serializable
case object GetRequest extends RequestType {
  override def get_? = true
}
@serializable
case object PostRequest extends RequestType {
  override def post_? = true
}
@serializable
case object HeadRequest extends RequestType {
  override def head_? = true
}
@serializable
case object PutRequest extends RequestType {
  override def put_? = true
}
@serializable
case object DeleteRequest extends RequestType {
  override def delete_? = true
}
@serializable
case class UnknownRequest(method: String) extends RequestType

object RequestType {
  def apply(req: HttpServletRequest): RequestType = {
    /*
    val ajax = req.getHeader("x-requested-with") match {
      case null => false
      case s @ _ => s.toUpperCase == "XMLHttpRequest".toUpperCase
    }*/
    
    req.getMethod.toUpperCase match {
      case "GET" => GetRequest
      case "POST" => PostRequest
      case "HEAD" => HeadRequest
      case "PUT" => PutRequest
      case "DELETE" => DeleteRequest
      case meth => UnknownRequest(meth)
    }
  }
}
