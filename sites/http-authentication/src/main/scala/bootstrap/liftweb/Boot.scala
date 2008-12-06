package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._

import _root_.net.liftweb.http.auth._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    LiftRules.addToPackages("net.liftweb.examples.authentication")

    val entries =
    Menu(Loc("Home", "index" :: Nil, "Home")) ::
    Menu(Loc("Secured", "secure" :: Nil, "Secured", Loc.HttpAuthProtected(() => Full(AuthRole("user"))))) ::
    Nil

    LiftRules.setSiteMap(SiteMap(entries: _*));



    // LiftRules.httpAuthProtectedResource = {
    //  case (ParsePath("secure" :: _, _, _, _)) => Full(AuthRole("user"))
    //}

    /**
     * This is the security function.
     * The URL's specified in protectedResource are secured by
     * this scheme.
     */
    LiftRules.authentication = HttpDigestAuthentication("lift") {
      /**
       * To verify, and see the resource, un: tim, pw: badger
       */
      case ("tim", req, func) => if (func("badger")) {
        println("AUTH OK")
        userRole(Full(AuthRole("user")))
	true
      } else {
	println("AUTH FAILED")
        false
      }
    }

    /**
    // if you want to use Basic authentication scheme then use this instead:

     LiftRules.authentication = HttpBasicAuthentication("lift") {
       case ("marius", "12test34", req) => println("marius is authenticated !"); userRole(Full(AuthRole("user"))); true;
     }
    */

  }
}

