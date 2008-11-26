package net.liftweb.http.auth

import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http._
import org.apache.commons.codec.binary._

/**
 * All http authentication methods must implement these methods.
 * The most important method to note here is the verified_? partial function
 * as this is what is used to then determine if the response specified in
 * the boot dispatcher is used or its a 401 response.
 *
 */
trait HttpAuthentication {
  
  def header(r: Req) = Can !! r.request.getHeader("Authorization")
  
  def verified_? : PartialFunction[Req, Can[UnauthorizedResponse]]
 
  def scheme : AuthenticationScheme = UnknownScheme
  
  def realm : String = ""
  
}

object NoAuthentication extends HttpAuthentication {
  def verified_? = {case req => Empty}
}

/**
 * Methods that are specific to HTTP basic are defined here.
 * The methods from the parent trait are implemented to decode the
 * Base64 encoded input from the http client.
 */
case class HttpBasicAuthentication(realmName: String)(func: PartialFunction[(String, String, Req), Boolean]) extends HttpAuthentication {
  
  def credentials(r: Req): Can[(String, String)] = {
    header(r).flatMap(auth => {
      val decoded = new String(Base64.decodeBase64(auth.substring(6,auth.length).getBytes)).split(":").toList
      decoded match {
        case userName :: password :: _ => Full((userName, password))
        case userName :: Nil => Full((userName, ""))
        case _ => Empty 
      }
    }
  )}
  
  override def scheme = BasicScheme
  
  override def realm = realmName
  
  def verified_? = {case (req) => {
    var cred = credentials(req)
    val ret = cred.map(t => 
      if (func.isDefinedAt(t._1, t._2, req))
        func(t._1, t._2, req)
      else
        false
    ) openOr false
    
    ret match {
      case false => Full(UnauthorizedResponse(realm))
      case _ => Empty
    }
  }}
  
}

case class HttpDigestAuthentication(realmName: String)(func: PartialFunction[(String, Req, (String) => Boolean), Boolean]) extends HttpAuthentication { 
  
  def getInfo(req: Req) : Can[DigestAuthentication] = header(req).map(auth => {
	 val info = auth.substring(7,auth.length)
     val pairs = splitNameValuePairs(info)
     DigestAuthentication(req.request.getMethod.toUpperCase, pairs("username"), pairs("realm"), pairs("nonce"),
	                      pairs("uri"), pairs("qop"), pairs("nc"),
	                      pairs("cnonce"), pairs("response"), pairs("opaque"))
    }
  )

  override def scheme = DigestScheme
  
  override def realm = realmName

  def verified_? = {case (req) => {
    var info = getInfo(req)
    val ret = info.map(t => 
      if (func.isDefinedAt((t.userName, req, validate(t) _)))
        func((t.userName, req, validate(t) _))
      else
        false
    ) openOr false
    
   ret match {
      case false => Full(UnauthorizedDigestResponse(realm, Qop.AUTH, randomString(64), randomString(64)))
      case _ => Empty
    }
    
  }}

  private def validate(clientAuth: DigestAuthentication)(password: String): Boolean = {
    val ha1 = hexEncode(md5((clientAuth.userName + ":" + clientAuth.realm + ":" + password).getBytes("UTF-8")))
    val ha2 = hexEncode(md5((clientAuth.method + ":" + clientAuth.uri).getBytes("UTF-8")))
    
    val response = hexEncode(md5((ha1 + ":" + clientAuth.nonce + ":" + 
                         clientAuth.nc + ":" + clientAuth.cnonce + ":" +
                         clientAuth.qop + ":" + ha2).getBytes("UTF-8")));

    response == clientAuth.response
  }
}

case class DigestAuthentication(method: String,
                                userName: String,
                                realm: String,
                                nonce: String,
                                uri: String,
                                qop: String,
                                nc: String,
                                cnonce: String,
                                response: String,
                                opaque: String)


sealed abstract class AuthenticationScheme {
  def code: String
  override def toString = "AuthenticationScheme(" + code + ")"
}
case object BasicScheme extends AuthenticationScheme {
  def code: String = "Basic"
}
case object DigestScheme extends AuthenticationScheme {
  def code: String = "Digest"
}
case object UnknownScheme extends AuthenticationScheme {
  def code: String = "Unknown"
}

sealed abstract class AuthenticationAlgorithm {
  def code: String
}
case object MD5Session extends AuthenticationAlgorithm {
  def code: String = "MD5-sess"
}
case object MD5 extends AuthenticationAlgorithm {
  def code: String = "MD5"
}
