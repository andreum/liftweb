package net.liftweb.http.js

/*                                                *\
(c) 2007 WorldWide Conferencing, LLC
Distributed under an Apache License
http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.xml._
import net.liftweb.util._
import Helpers._

import JE._
import JsCmds._

trait JxYieldFunc {
  this: JxBase =>
  def yieldFunction: JsExp
}

trait JxBase {
  self: Node =>

  def appendToParent(parentName: String): JsCmd

  def label = throw new UnsupportedOperationException("Xml2Js does not have a label")

  def addAttrs(varName: String, attrs: List[MetaData]): JsCmd = attrs.map {
    m =>
    m.value.map{
      case exp: JsExp =>
      JsRaw( varName+"."+m.key+" = "+exp.toJsCmd).cmd

      case cmd: JsCmd => val varName = "v"+randomString(20)
      JsCrVar(varName, AnonFunc(cmd)) &
      JsRaw(varName+"."+m.key+" = "+varName+"()")

      case JxAttr(cmd) =>
      JsRaw(varName+"."+m.key+" = "+ cmd.toJsCmd).cmd

      case JxFuncAttr(cmd) =>
      JsRaw(varName+"."+m.key+" = "+ AnonFunc(cmd).toJsCmd).cmd

      case x =>
      if (m.key == "class") {
        // JsRaw(varName+".setAttribute('className',"+x.text.encJs+");").cmd

        JsRaw(varName+".className = "+x.text.encJs).cmd &
        JsRaw(varName+".setAttribute("+m.key.encJs+","+x.text.encJs+");").cmd
      } else {
      JsRaw(varName+".setAttribute("+m.key.encJs+","+x.text.encJs+");").cmd
      }
    }.foldLeft(Noop)(_ & _)
  }.foldLeft(Noop)(_ & _)

  private def fixText(in: String): String = (in, in.trim) match {
    case (x, y) if x == y => x
    case (x, y) if x startsWith y => y + " "
    case (x, y) if y.length == 0 => " "
    case (x, y) if x endsWith y => " "+y
    case (_, y) => " "+y+" "
  }

  def addToDocFrag(parent: String, elems: List[Node]): JsCmd = elems.map{
    case Jx(kids) => addToDocFrag(parent, kids.toList)
    case jb: JxBase => jb.appendToParent(parent)
    case Group(nodes) => addToDocFrag(parent, nodes.toList)
    case Text(txt) => JsRaw(parent+".appendChild(document.createTextNode("+fixText(txt).encJs+"));").cmd
    case a: Atom[_] => JsRaw(parent+".appendChild(document.createTextNode("+a.text.encJs+"));").cmd
    case e: scala.xml.Elem =>
    val varName = "v"+randomString(10)
    JsCrVar(varName, JsRaw("document.createElement("+e.label.encJs+")")) &
    addAttrs(varName, e.attributes.toList) &
    JsRaw(parent+".appendChild("+varName+")") &
    addToDocFrag(varName, e.child.toList)
    case ns: Seq[Node] =>
    if (ns.length == 0) Noop
    else if (ns.length == 1) {
      Log.error("In addToDocFrag, got a "+ns+" of type "+ns.getClass.getName)
      Noop
    } else addToDocFrag(parent, ns.toList)

  }.foldLeft(Noop)(_ & _)
}


abstract class JxNodeBase extends Node with JxBase {

}

case class JxAttr(in: JsCmd) extends Node with JxBase {
  def child = Nil

  def appendToParent(parentName: String): JsCmd = {
    Noop
  }
}

case class JxFuncAttr(in: JsCmd) extends Node with JxBase {
  def child = Nil

  def appendToParent(parentName: String): JsCmd = {
    Noop
  }
}

case class JxMap(in: JsExp, what: JxYieldFunc) extends Node with JxBase {
  def child = Nil

  def appendToParent(parentName: String): JsCmd = {
    val ran = "v"+randomString(10)
    val fr = "f"+randomString(10)
    val cr = "c"+randomString(10)
    JsCrVar(ran, in) &
    JsCrVar(fr, what.yieldFunction) &
    JsRaw("for ("+cr+" = 0; "+cr+" < "+ran+".length; "+cr+"++) {"+
    parentName+".appendChild("+fr+"("+ran+"["+cr+"]));"+
    "}")
  }
}

case class JxCmd(in: JsCmd) extends Node with JxBase {
  def child = Nil

  def appendToParent(parentName: String) = in
}

case class JxMatch(exp: JsExp, cases: JxCase*) extends Node with JxBase {
  def child = Nil

  def appendToParent(parentName: String): JsCmd = {
    val vn = "v" + randomString(10)
    JsCrVar(vn, exp) &
    JsRaw("if (false) {\n} "+
    cases.map{c =>
      " else if ("+vn+" == "+c.toMatch.toJsCmd+") {"+
      addToDocFrag(parentName, c.toDo.toList).toJsCmd+
      "\n}"
    }.mkString("")+
    " else {throw new Exception('Unmatched: '+"+vn+");}")
  }
}

case class JxCase(toMatch: JsExp, toDo: NodeSeq)

case class JxIf(toTest: JsExp, ifTrue: NodeSeq) extends Node with JxBase {
  def child = Nil
  def appendToParent(parentName: String): JsCmd = {
    JsRaw("if ("+toTest.toJsCmd+") {\n"+
    addToDocFrag(parentName, ifTrue.toList).toJsCmd+
    "}\n")
  }
}

case class JxIfElse(toTest: JsExp, ifTrue: NodeSeq, ifFalse: NodeSeq) extends Node with JxBase {
  def child = Nil
  def appendToParent(parentName: String): JsCmd = {
    JsRaw("if ("+toTest.toJsCmd+") {\n"+
    addToDocFrag(parentName, ifTrue.toList).toJsCmd+
    "} else {\n" +
    addToDocFrag(parentName, ifFalse.toList).toJsCmd+
    "}\n")
  }
}



case class Jx(child: NodeSeq) extends Node with JxBase with JxYieldFunc {

  def appendToParent(parentName: String): JsCmd =
  addToDocFrag(parentName,child.toList)

  def yieldFunction: JsExp = toJs

  def toJs: JsExp = AnonFunc("it",
  JsCrVar("df", JsRaw("document.createDocumentFragment()")) &
  addToDocFrag("df", child.toList) &
  JsRaw("return df"))


}

