package net.liftweb.http

/*                                                *\
 (c) 2008 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import _root_.scala.xml.Node
import _root_.net.liftweb.util._
import _root_.javax.servlet.http.Cookie

/**
 * 401 Unauthorized Response.
 */
case class UnauthorizedResponse(realm: String) extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), List("WWW-Authenticate" -> ("Basic realm=\"" + realm + "\"")), Nil, 401)
}

/**
 * 301 Redirect.
 */
case class PermRedirectResponse(uri: String, request: Req, cookies: Cookie*) extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), List("Location" -> request.updateWithContextPath(uri)), cookies.toList, 301)
}

/**
 * Returning an Atom category document.
 */
case class AtomCategoryResponse(xml: Node) extends LiftResponse {
  def toResponse = XmlMimeResponse(xml, "application/atomcat+xml").toResponse
}

/**
 * Returning an Atom Service Document.
 */
case class AtomServiceResponse(xml: Node) extends LiftResponse {
  def toResponse = XmlMimeResponse(xml, "application/atomsvc+xml").toResponse
}

/**
 * Allows you to create custom 200 responses for clients using different
 * Content-Types.
 */
case class XmlMimeResponse(xml: Node, mime: String) extends ToResponse {
  def docType = Empty
  def code = 200
  def headers = List("Content-Type" -> mime)
  def cookies = Nil
  def out = xml
}

/**
 * Your Request was missing an important element. Use this as a last resort if
 * the request appears incorrect.
 */
case class BadResponse extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), Nil, Nil, 400)
}

/**
 * The Resource was created. We then return the resource, post-processing, to
 * the client.
 */
case class CreatedResponse(xml: Node, mime: String) extends ToResponse {
  def docType = Empty
  def code = 201
  def headers = List("Content-Type" -> mime)
  def cookies = Nil
  def out = xml
}

/**
 * Returning an Atom document.
 */
case class AtomResponse(xml: Node) extends ToResponse {
  def docType = Empty
  def code = 200
  def headers = List("Content-Type" -> "application/atom+xml")
  def cookies = Nil
  def out = xml
}

/**
 * Returning an OpenSearch Description Document.
 */
case class OpenSearchResponse(xml: Node) extends ToResponse {
  def docType = Empty
  def code = 200
  def headers = List("Content-Type" -> "application/opensearchdescription+xml")
  def cookies = Nil
  def out = xml
}

/**
 * The Atom entity was successfully created and is shown to the client.
 */
case class AtomCreatedResponse(xml: Node) extends LiftResponse {
  def toResponse = CreatedResponse(xml, "application/atom+xml").toResponse
}

object PlainTextResponse {
  def apply(text: String): PlainTextResponse = PlainTextResponse(text, Nil, 200)
  def apply(text: String, code: Int): PlainTextResponse = PlainTextResponse(text, Nil, code)
}

case class PlainTextResponse(text: String, headers: List[(String, String)], code: Int) extends LiftResponse {
    def toResponse = {
        val bytes = text.getBytes("UTF-8")
        InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/plain") :: headers, Nil, code)
    }
}

object CSSResponse {
  def apply(text: String): CSSResponse = CSSResponse(text, Nil, 200)
  def apply(text: String, code: Int): CSSResponse = CSSResponse(text, Nil, code)
}

case class CSSResponse(text: String, headers: List[(String, String)], code: Int) extends LiftResponse {
    def toResponse = {
        val bytes = text.getBytes("UTF-8")
        InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/css") :: headers, Nil, code)
    }
}

/**
 * Basic 200 response but without body.
 */
case class OkResponse extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), Nil, Nil, 200)
}

/**
 * This Resource does not allow this method. Use this when the resource can't
 * understand the method no matter the circumstances.
 */
case class MethodNotAllowedResponse extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), Nil, Nil, 405)
}

/**
 * The requested Resource does not exist.
 */
case class NotFoundResponse extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), Nil, Nil, 404)
}

/**
 * The requested Resource used to exist but no longer does.
 */
case class GoneResponse extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), Nil, Nil, 410)
}
