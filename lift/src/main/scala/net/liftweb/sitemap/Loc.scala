package net.liftweb.sitemap

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

import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import Helpers._
import auth._

import _root_.scala.xml.{NodeSeq, Text}

sealed trait NullLocParams
object NullLocParams extends NullLocParams

/**
 * A menu location
 */
trait Loc[ParamType] {
  type LocRewrite =  Can[PartialFunction[RewriteRequest, (RewriteResponse, ParamType)]]

  def name: String

  def link: Loc.Link[ParamType]

  def text: Loc.LinkText[ParamType]

  def params: List[Loc.LocParam]

  def defaultParams: Can[ParamType]

  def rewrite: LocRewrite = Empty

  def createDefaultLink: Option[NodeSeq] = (foundParam.is or defaultParams).flatMap(p => link.createLink(p)).toOption

  def createLink(in: ParamType): Option[NodeSeq] = link.createLink(in).toOption

  override def toString = "Loc("+name+", "+link+", "+text+", "+params+")"

  def rewritePf: Can[LiftRules.RewritePf] = rewrite.map(rw =>
    new AnyRef with PartialFunction[RewriteRequest, RewriteResponse] {
      def isDefinedAt(in: RewriteRequest) = rw.isDefinedAt(in)

      def apply(in: RewriteRequest): RewriteResponse = {
        val (ret, param) = rw.apply(in)
        foundParam.set(Full(param))
        ret
      }
    }
  )

  type SnippetTest = PartialFunction[(String, Can[ParamType]), NodeSeq => NodeSeq]

  def snippets: SnippetTest = Map.empty

  lazy val calcSnippets: SnippetTest = {
    def buildPf(in: Loc.Snippet): PartialFunction[String, NodeSeq => NodeSeq] =
    new PartialFunction[String, NodeSeq => NodeSeq] {
      def isDefinedAt(s: String) = s == in.name
      def apply(s: String): NodeSeq => NodeSeq =
      if (isDefinedAt(s)) in.func
      else throw new MatchError()
    }

    val singles = params.flatMap{case v: Loc.Snippet => Some(v)
      case _ => None}.toList.map(buildPf) :::
    params.flatMap{case v: Loc.LocSnippets => Some(v)
      case _ => None}.toList

    if (singles.isEmpty) Map.empty
    else {
      val func: PartialFunction[String, NodeSeq => NodeSeq] = singles match {
        case pf :: Nil => pf
        case pfs => pfs.reduceLeft[PartialFunction[String, NodeSeq => NodeSeq]](_ orElse _)
      }

      new SnippetTest {
        def isDefinedAt(in: (String, Can[ParamType])): Boolean =
        func.isDefinedAt(in._1)

        def apply(in: (String, Can[ParamType])): NodeSeq => NodeSeq =
        func.apply(in._1)
      }
    }
  }

  def snippet(name: String): Can[NodeSeq => NodeSeq] = {
    val test = (name, foundParam.is)

    if ((snippets orElse calcSnippets).isDefinedAt(test)) Full((snippets orElse calcSnippets)(test))
    else Empty
  }

  def testAccess: Either[Boolean, Can[LiftResponse]] = {
    def testParams(what: List[Loc.LocParam]): Either[Boolean, Can[LiftResponse]] = what match {
      case Nil => Left(true)

      case Loc.If(test, msg) :: xs =>
        if (!test()) Right(Full(msg()))
        else testParams(xs)

      case Loc.Unless(test, msg) :: xs =>
        if (test()) Right(Full(msg()))
        else testParams(xs)

      case x :: xs => testParams(xs)
    }

    testParams(params) match {
      case Left(true) => _menu.testParentAccess
      case x => x
    }
  }

  /**
   * Is there a template assocaited with this Loc?
   */
  def template: Can[NodeSeq] =
  paramTemplate.map(_.template()) or calcTemplate

  /**
   * A method that can be override to provide a template for this Loc
   */
  def calcTemplate: Can[NodeSeq] = Empty

  /**
   * Look for the Loc.Template in the param list
   */
  lazy val paramTemplate: Can[Loc.Template] =
  params.flatMap{case v: Loc.Template => Some(v) case _ => None}.firstOption


  private def findTitle(lst: List[Loc.LocParam]): Can[Loc.Title[ParamType]] = lst match {
    case Nil => Empty
    case (t : Loc.Title[ParamType]) :: xs => Full(t)
    case _ => findTitle(lst.tail)
  }

  /**
   * The title of the location
   */
  def title: NodeSeq = (foundParam.is or defaultParams).map(p => title(p)) openOr Text(name)

  def title(in: ParamType): NodeSeq = findTitle(params).map(_.title(in)) openOr text.text(in)

  def linkText: Can[NodeSeq] = (foundParam.is or defaultParams).map(p => linkText(p))

  def linkText(in: ParamType): NodeSeq = text.text(in)

  private[sitemap] def setMenu(p: Menu)
  {
    _menu = p
    p.siteMap.addLoc(this)
  }

  protected object foundParam extends RequestVar[Can[ParamType]](Empty) {
    override val __nameSalt = randomString(10)
  }

  private var _menu: Menu = _
  def menu = _menu

  private def testAllParams(what: List[Loc.LocParam], req: Req): Boolean = {
    what match {
      case Nil => true
      case (x: Loc.Test) :: xs =>
        if (!x.test(req)) false
        else testAllParams(xs, req)

      case x :: xs => testAllParams(xs, req)
    }
  }

  def doesMatch_?(req: Req): Boolean =
  if (link.isDefinedAt( req ) ) {
    link(req) match {
      case Full(x) if testAllParams(params, req) => x
      case Full(x) => false
      case x => x.openOr(false)
    }
  } else false

  def breadCrumbs: List[Loc[_]] = _menu.breadCrumbs ::: List(this)

  def buildMenu: CompleteMenu = {
    val theKids = _menu.kids.toList.flatMap(_.loc.buildItem(Nil, false, false))

    CompleteMenu(_menu.buildUpperLines(_menu, _menu, theKids))
  }

  private[sitemap] def buildItem(kids: List[MenuItem], current: Boolean, path: Boolean): Can[MenuItem] =
  (hidden, testAccess) match {
    case (false, Left(true)) =>
      defaultParams.flatMap(p =>
        link.createLink(p).map(t =>
          MenuItem(text.text(p),t, kids, current, path,
                   params.flatMap {
              case v: Loc.LocInfo[(T forSome {type T})] => v()
              case _ =>  Empty
            }
          )))

    case _ => Empty
  }

  private lazy val hidden = params.contains(Loc.Hidden)

  private lazy val groupSet: Set[String] =
  Set(params.flatMap{case s: Loc.LocGroup => s.group case _ => Nil} :_*)

  def inGroup_?(group: String): Boolean = groupSet.contains(group)
}

/**
 * The Loc companion object, complete with a nice constructor
 */
object Loc {
  type FailMsg = () => LiftResponse

  /**
   * Create a Loc (Location) instance
   *
   * @param name -- the name of the location.  This must be unique across your entire sitemap.
   * It's used to look up a menu item in order to create a link to the menu on a page.
   * @param link -- the Link to the page
   * @param text -- the text to display when the link is displayed
   * @param params -- access test, title calculation, etc.
   *
   */
  def apply(theName: String,
            theLink: Link[NullLocParams],
            theText: LinkText[NullLocParams],
            theParams: LocParam*): Loc[NullLocParams] =
  new Loc[NullLocParams] {
    val name = theName
    val link: Loc.Link[NullLocParams] = theLink

    val text: Loc.LinkText[NullLocParams] = theText
    val defaultParams: Can[NullLocParams] = Full(NullLocParams)

    val params: List[LocParam] = theParams.toList

    checkProtected(link, params)
  }

  def apply(theName: String, theLink: Loc.Link[NullLocParams],
            theText: LinkText[NullLocParams],
            theParams: List[LocParam]): Loc[NullLocParams] =
  new Loc[NullLocParams] {
    val name = theName
    val link: Loc.Link[NullLocParams] = theLink
    val defaultParams: Can[NullLocParams] = Full(NullLocParams)

    val text: Loc.LinkText[NullLocParams] = theText

    val params: List[LocParam]  = theParams.toList

    checkProtected(link, params)
  }

  def checkProtected(link: Link[_], params: List[LocParam]) {
    params.map(lp => {
      lp match {
        case Loc.HttpAuthProtected(role) => LiftRules.addHttpAuthProtectedResource(
          new LiftRules.HttpAuthProtectedResourcePf() {
			def isDefinedAt(in: ParsePath) = in.partPath == link.uriList
			def apply(in: ParsePath): Can[Role] = Full(role())
          })
       case _ => lp
      }})
  }

  trait LocParam
  /**
   * A title for the page.  A function that calculates the title... useful
   * if the title of the page is dependent on current state
   */
  case class Title[T](title: T => NodeSeq) extends LocParam

  /**
   * If this parameter is included, the item will not be visible in the menu, but
   * will still be accessable.
   */
  case object Hidden extends LocParam

  /**
   * Indicates that the path denominated by Loc requires HTTP authentication
   * and only a user assigned to this role or to a role that is child-of this role
   * can access it.
   */
  case class HttpAuthProtected(role: () => Role) extends LocParam

  /**
   * If the Loc is in a group (or groups) like "legal" "community" etc.
   * the groups can be specified and recalled at the top level
   */
  case class LocGroup(group: String*) extends LocParam

  /**
   * If the test returns True, the page can be accessed, otherwise,
   * the result of FailMsg will be sent as a response to the browser.
   * If the Loc cannot be accessed, it will not be displayed in menus.
   *
   * @param test -- the function that tests access to the page
   * @param failMsg -- what to return the the browser (e.g., 304, etc.) if
   * the page is accessed.
   */
  case class If(test: () => Boolean, failMsg: FailMsg) extends LocParam

  /**
   * Unless the test returns True, the page can be accessed, otherwise,
   * the result of FailMsg will be sent as a response to the browser.
   * If the Loc cannot be accessed, it will not be displayed in menus.
   *
   * @param test -- the function that tests access to the page
   * @param failMsg -- what to return the the browser (e.g., 304, etc.) if
   * the page is accessed.
   */
  case class Unless(test: () => Boolean, failMsg: FailMsg) extends LocParam

  /**
   * Tests to see if the request actually matches the requirements for access to
   * the page.  For example, if a parameter is missing from the request, this
   * is a good way to restrict access to the page.
   */
  case class Test(test: Req => Boolean) extends LocParam

  /**
   * A single snippet that's assocaited with a given location... the snippet
   * name and the snippet function'
   */
  case class Snippet(name: String, func: NodeSeq => NodeSeq) extends LocParam

  case class Template(template: () => NodeSeq) extends LocParam

  /**
   * Allows you to create a handler for many snippets that are associated with
   * a Loc
   */
  trait LocSnippets extends PartialFunction[String, NodeSeq => NodeSeq] with LocParam

  /**
   * A subclass of LocSnippets with a built in dispatch method (no need to
   * implement isDefinedAt or apply... just
   * def dispatch: PartialFunction[String, NodeSeq => NodeSeq]
   */
  trait DispatchLocSnippets extends LocSnippets {
    def dispatch: PartialFunction[String, NodeSeq => NodeSeq]

    def isDefinedAt(n: String) = dispatch.isDefinedAt(n)

    def apply(n: String) = dispatch.apply(n)
  }

  /**
   * What's the link text.
   */
  case class LinkText[T](text: T => NodeSeq)

  /**
   * This defines the Link to the Loc.
   *
   * @param uri -- the relative (to parent menu item) or absolute path
   * to match for this Loc. <br />
   * "/foo" -- match the "foo" file <br/>
   * "foo" -- match the foo file in the directory defined by the parent Menu
   * @param matchOnPrefix -- false -- absolute match.  true -- match anything
   * that begins with the same path.  Useful for opening a set of directories
   * (for example, help pages)
   * @param create -- create a URL based on incoming parameters
   */
  class Link[T](val uriList: List[String], val matchHead_? : Boolean) extends
  PartialFunction[Req, Can[Boolean]] {
    def this(b: List[String]) = this(b, false)

    def isDefinedAt(req: Req): Boolean =
    if (matchHead_?) req.path.partPath.take(uriList.length) == uriList
    else uriList == req.path.partPath

    def apply(in: Req): Can[Boolean] = if (isDefinedAt(in)) Full(true)
    else throw new MatchError("Failed for Link "+uriList)


    def createLink(params: T): Can[NodeSeq] =
    Full(Text(uriList.mkString("/", "/", "")))
  }

  /**
   * A companion object to create some variants on Link
   */
  object Link {
    def apply(urlLst: List[String], matchHead_? : Boolean, url: String) =
    new Link[NullLocParams](urlLst, matchHead_?) {
      override def createLink(params: NullLocParams): Can[NodeSeq] =
      Full(Text(url))
    }

  }

  object ExtLink {
    def apply(url: String) = new Link[NullLocParams](Nil, false) {
      override def createLink(params: NullLocParams): Can[NodeSeq] =
      Full(Text(url))
    }
  }

  trait LocInfoVal[T] {
    def value: T
  }

  trait LocInfo[T] extends LocParam {
    def apply(): Can[LocInfoVal[T]]
  }

  def alwaysTrue(a: Req) = true
  def retString(toRet: String)(other: Seq[(String, String)]) = Full(toRet)

  implicit def nodeSeqToLinkText[T](in: => NodeSeq): LinkText[T] = LinkText[T](T => in)
  implicit def strToLinkText[T](in: => String): LinkText[T] = LinkText(T => Text(in))
  implicit def strLstToLink(in: Seq[String]): Link[NullLocParams] = new Link[NullLocParams](in.toList)
  implicit def strPairToLink(in: (Seq[String], Boolean)): Link[NullLocParams] = new Link[NullLocParams](in._1.toList, in._2)
  implicit def strToFailMsg(in: String): FailMsg =
  f(RedirectWithState(LiftRules.siteMapFailRedirectLocation.
                      mkString("/", "/", ""),
                      RedirectState(Empty, in -> NoticeType.Error)))
  implicit def redirectToFailMsg(in: RedirectResponse): FailMsg = f(in)

  def f(in: String): () => String = () => in
  def f(in: RedirectResponse): () => RedirectResponse = () => in
}



case class CompleteMenu(lines: Seq[MenuItem]) {
  lazy val breadCrumbs: Seq[MenuItem] = lines.flatMap(_.breadCrumbs)
}

case class MenuItem(text: NodeSeq, uri: NodeSeq,  kids: Seq[MenuItem],
                    current: Boolean,
                    path: Boolean,
                    info: List[Loc.LocInfoVal[(T forSome {type T})]])
{
  def breadCrumbs: Seq[MenuItem] = if (!path) Nil
  else this :: kids.toList.flatMap(_.breadCrumbs)
}
