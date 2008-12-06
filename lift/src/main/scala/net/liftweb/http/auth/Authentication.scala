package net.liftweb.http.auth

import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http._
import _root_.org.apache.commons.codec.binary._
import _root_.scala.collection.mutable.{HashMap}
import _root_.scala.actors.Actor
import _root_.scala.actors.Actor._

/**
 * All http authentication methods must implement these methods.
 * The most important method to note here is the verified_? partial function
 * as this is what is used to then determine if the response specified in
 * the boot dispatcher is used or its a 401 response.
 *
 */
trait HttpAuthentication {

  def header(r: Req) = Can !! r.request.getHeader("Authorization")

  def verified_? : PartialFunction[Req, Boolean]

  def realm : String = ""

  def unauthorizedResponse: UnauthorizedResponse = UnauthorizedResponse(realm)

  def shutDown {}

}

object NoAuthentication extends HttpAuthentication {
  def verified_? = {case req => true}
}

object userRole extends RequestVar[Can[Role]](Empty)

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

  override def realm = realmName

  def verified_? = {case (req) => {
    credentials(req) match {
      case Full((user, pwd)) if (func.isDefinedAt(user, pwd, req)) =>
        func(user, pwd, req)
      case _ => false
    }
   }
  }

}

case class HttpDigestAuthentication(realmName: String)(func: PartialFunction[(String, Req, (String) => Boolean), Boolean]) extends HttpAuthentication {
  private val nonceMap = new HashMap[String, Long]

  private object CheckAndPurge
  private object ShutDown

  object NonceWatcher extends Actor {
    def act = {
      doPing()
      loop {
        react {
         case CheckAndPurge => nonceMap.foreach((entry) => {
           val ts = System.currentTimeMillis
           if ((ts - entry._2) > nonceValidityPeriod) {
             nonceMap -= entry._1
           }
         })
         case ShutDown => self.exit("Terminating nonce actor");
        }
      }
    }

   private def doPing() {
     try {
       ActorPing scheduleAtFixedRate(this, CheckAndPurge, 0, 5 seconds)
     } catch {
       case e => Log.error("Couldn't start NonceWatcher ping", e)
    }
   }

  }

  NonceWatcher.start

  override def shutDown = NonceWatcher ! ShutDown

  def getInfo(req: Req) : Can[DigestAuthentication] = header(req).map(auth => {

	 val info = auth.substring(7,auth.length)
     val pairs = splitNameValuePairs(info)
     DigestAuthentication(req.request.getMethod.toUpperCase, pairs("username"), pairs("realm"), pairs("nonce"),
	                      pairs("uri"), pairs("qop"), pairs("nc"),
	                      pairs("cnonce"), pairs("response"), pairs("opaque"))
    }
  )

  /**
   * The period in milli seconds during which the nonce sent by server is valid. After this period
   * even if the auth digest matches correctly the authentication will fail.
   *
   * A useful usability would be to return something like "5 seconds" where seconds function is defined in TimeHelpers.
   * The default value returned is 30 seconds.
   *
   */
  def nonceValidityPeriod: Long = 30 seconds

  override def realm = realmName

  override def unauthorizedResponse = {
    val nonce = randomString(64);
    nonceMap += (nonce -> System.currentTimeMillis)
    UnauthorizedDigestResponse(realm, Qop.AUTH, nonce, randomString(64))
  }

  def verified_? = {case (req) => {
    getInfo(req) match {
      case Full(auth) if (func.isDefinedAt((auth.userName, req, validate(auth) _))) =>
        func((auth.userName, req, validate(auth) _)) match {
          case true =>
            val ts = System.currentTimeMillis
            val nonceCreationTime: Long = nonceMap.getOrElse(auth.nonce, -1)
            nonceCreationTime match {
              case -1 => false
              case _ =>
                (ts - nonceCreationTime) < nonceValidityPeriod
            }
          case _ => false
        }
      case _ => false
    }
  }}

  private def validate(clientAuth: DigestAuthentication)(password: String): Boolean = {
    val ha1 = hexEncode(md5((clientAuth.userName + ":" + clientAuth.realm + ":" + password).getBytes("UTF-8")))
    val ha2 = hexEncode(md5((clientAuth.method + ":" + clientAuth.uri).getBytes("UTF-8")))

    val response = hexEncode(md5((ha1 + ":" + clientAuth.nonce + ":" +
                         clientAuth.nc + ":" + clientAuth.cnonce + ":" +
                         clientAuth.qop + ":" + ha2).getBytes("UTF-8")));

    (response == clientAuth.response) && (nonceMap.getOrElse(clientAuth.nonce, -1) != -1)
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
