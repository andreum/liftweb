/*
 * Copyright 2008 WorldWide Conferencing, LLC
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
package bootstrap.liftweb

import java.util.Locale

import javax.servlet.http.HttpServletRequest

import net.liftweb.util.{Box,Empty,Full,LoanWrapper,LogBoot}
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftweb.jpademo.model._
import S.?

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    LogBoot.defaultProps =
      """<?xml version="1.0" encoding="UTF-8" ?>
      <!DOCTYPE log4j:configuration SYSTEM "log4j.dtd">
      <log4j:configuration xmlns:log4j="http://jakarta.apache.org/log4j/">
      <appender name="appender" class="org.apache.log4j.ConsoleAppender">
      <layout class="org.apache.log4j.SimpleLayout"/>
      </appender>
      <root>
      <priority value ="DEBUG"/>
      <appender-ref ref="appender"/>
      </root>
      </log4j:configuration>
      """

    // where to search snippet
    LiftRules.addToPackages("net.liftweb.jpademo")

    // Set up a site map
    val entries = SiteMap(Menu(Loc("Home", "index" :: Nil , ?("Home"))),
			  Menu(Loc("Authors", "authors" :: "list" :: Nil, ?("Author List"))),
			  Menu(Loc("Add Author", "authors" :: "add" :: Nil, ?("Add Author"), Hidden)),
			  Menu(Loc("Books", "books" :: "list" :: Nil, ?("Book List"))),
			  Menu(Loc("Add Book", "books" :: "add" :: Nil, ?("Add Book"), Hidden)),
			  Menu(Loc("BookSearch", "books" :: "search" :: Nil, ?("Book Search"))))

    LiftRules.setSiteMap(entries)

    // And now, for a little fun :)
    val swedishChef = new Locale("chef")

    object swedishOn extends SessionVar(false)

    def localeCalculator (request : Box[HttpServletRequest]) : Locale =
      request.flatMap(_.getParameter("swedish") match {
	case null if swedishOn.is == true => Full(swedishChef)
	case null => Full(LiftRules.defaultLocaleCalculator(request))
        case "true" => { swedishOn(true); Full(swedishChef) }
        case "false" => { swedishOn(false); Full(LiftRules.defaultLocaleCalculator(request)) }
      }).openOr(Locale.getDefault())

    LiftRules.localeCalculator = localeCalculator _
  }
}

