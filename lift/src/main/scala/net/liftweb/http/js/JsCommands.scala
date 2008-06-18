package net.liftweb.http.js

/*                                                *\
(c) 2007 WorldWide Conferencing, LLC
Distributed under an Apache License
http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.xml.{NodeSeq, Group, Unparsed, Elem}
import net.liftweb.util.Helpers._
import net.liftweb.util.Helpers
import net.liftweb.util.TimeHelpers
import net.liftweb.util._
import scala.xml.{Node, SpecialNode}

object JsCommands {
  def create = new JsCommands(Nil)  
  def apply(in: Seq[JsCmd]) = new JsCommands(in.toList.reverse)
}

class JsCommands(val reverseList: List[JsCmd]) extends ResponseIt {
  def &(in: JsCmd) = new JsCommands(in :: reverseList)
  def &(in: List[JsCmd]) = new JsCommands(in.reverse ::: reverseList)
  
  def toResponse = {
    val data = reverseList.reverse.map(_.toJsCmd).mkString("\n").getBytes("UTF-8")
    Response(data, List("Content-Length" -> data.length.toString, "Content-Type" -> "text/javascript"), Nil, 200)
  }
}

case class JsonCall(funcId: String) {
  def exp(exp: JsExp): JsCmd = JsCmds.Run(funcId+"("+exp.toJsCmd+");")
  
  def apply(command: String): JsCmd = apply(JE.Str(command))
  
  def apply(command: JsExp): JsCmd = 
  JsCmds.Run(funcId+"({'command': "+command.toJsCmd+", 'params': false});")
  
  def apply(command: String, params: JsExp) = 
  JsCmds.Run(funcId+"({'command': "+command.encJs+", 'params':"+
  params.toJsCmd+"});")
  
  def apply(command: String, target: String, params: JsExp) =
  JsCmds.Run(funcId+"({'command': "+command.encJs+", 'target': "+
  target.encJs+
  ", 'params':"+
  params.toJsCmd+"});")
  
  
  def apply(command: JsExp, params: JsExp) = 
  JsCmds.Run(funcId+"({'command': "+command.toJsCmd+", 'params':"+
  params.toJsCmd+"});")
  
  def apply(command: JsExp, target: JsExp, params: JsExp) =
  JsCmds.Run(funcId+"({'command': "+command.toJsCmd+", 'target': "+
  target.toJsCmd+
  ", 'params':"+
  params.toJsCmd+"});")
  
}

trait JsExp extends SpecialNode with HtmlFixer with JxBase {
  def toJsCmd: String
  
  // def label: String = "#JS"
  
  override def toString(sb: StringBuilder) = {
    sb.append("<!-- ")
    sb.append(toJsCmd)
    sb.append("\n-->")
    sb
  }
  
  def appendToParent(parentName: String): JsCmd = {
    val ran = "v"+randomString(10)
    JsCmds.JsCrVar(ran, this) &
    JE.JsRaw("if ("+ran+".parentNode) "+ran+" = "+ran+".cloneNode(true)").cmd &
    JE.JsRaw("if ("+ran+".nodeType) {"+parentName+".appendChild("+ran+");} else {"+
    parentName+".appendChild(document.createTextNode("+ran+"));}").cmd
  }
  
  def !(right: JsMethod): JsExp = new JsExp {
    def toJsCmd = JsExp.this.toJsCmd + "." + right.toJsCmd
  }
  
  /**
   * --> accesses a property in the current JsExp
   */
  def -->(right: JsMethod): JsExp = this ! right

  /*
  def :=(right: JsExp): JsExp = new JsExp {
    def toJsCmd = JsExp.this.toJsCmd +" = " +right.toJsCmd
  }*/
  
  def cmd: JsCmd = JsCmds.Run(toJsCmd+";")
  
  
  def +(right: JsExp): JsExp = new JsExp {
    def toJsCmd = JsExp.this.toJsCmd + " + "+ right.toJsCmd
  }
  
}

trait JsMethod {
  def toJsCmd: String
}

/**
 * JavaScript Expressions. To see these in action, check out 
 * sites/example/src/webapp/json.html
 */
object JE {
  implicit def strToS(in: String): Str = Str(in)
  implicit def boolToJsExp(in: Boolean): JsExp = if (in) JsTrue else JsFalse
  implicit def numToJsExp(in: Int): JsExp = Num(in)
  implicit def numToJsExp(in: Long): JsExp = Num(in)
  implicit def numToJsExp(in: Double): JsExp = Num(in)
  implicit def numToJsExp(in: Float): JsExp = Num(in)
  // implicit def setExToArray(in: Seq[JsExp]): JsArray[JsExp] = JsArray[JsExp](in :_*)
  
  case class Num(n: Number) extends JsExp {
    def toJsCmd = n.toString
  }
  
  case class Stringify(in: JsExp) extends JsExp {
    def toJsCmd = "JSON.stringify("+in.toJsCmd+")"
  }
  
  object JsArray {
    def apply(in: JsExp*): JsExp = new JsExp {
      def toJsCmd = in.map(_.toJsCmd).mkString("[",", ", "]\n")
    }
    
    def apply(in: List[JsExp]): JsExp = this.apply(in :_*)
    
  }
  
  case class ValById(id: String) extends JsExp {
    def toJsCmd = "document.getElementById("+id.encJs+").value"
  }

  /**
   * gets the element by ID
   */
  case class ElemById(id: String, then: String*) extends JsExp {
    override def toJsCmd = "document.getElementById("+id.encJs+")" + (
    if (then.isEmpty) "" else then.mkString(".", ".", "")
    )
  }
  
  object LjSwappable {
    def apply(visible: JsExp, hidden: JsExp): JxBase = {
      new JxNodeBase {
        def child = Nil
        def appendToParent(name: String): JsCmd = 
        JsRaw(name+".appendChild(lift$.swappable("+visible.toJsCmd
        +", "+hidden.toJsCmd +"))").cmd
      }
    }
    
    def apply(visible: NodeSeq, hidden: NodeSeq): JxBase = {
      new JxNodeBase {
        def child = Nil
        def appendToParent(name: String): JsCmd = 
        JsRaw(name+".appendChild(lift$.swappable("+AnonFunc(
          JsCmds.JsCrVar("df", JsRaw("document.createDocumentFragment()")) &
          addToDocFrag("df", visible.toList) &
          JE.JsRaw("return df").cmd
        ).toJsCmd
        +"(), "+AnonFunc(JsCmds.JsCrVar("df", JsRaw("document.createDocumentFragment()")) &
          addToDocFrag("df", hidden.toList) &
          JE.JsRaw("return df").cmd).toJsCmd +"()))").cmd
      }
    }
  }
   
  object LjBuildIndex {
    def apply(obj: String,
    indexName: String, tables: (String, String)*): JsExp = new JsExp {
      def toJsCmd = "lift$.buildIndex("+obj+", "+indexName.encJs+
      (if (tables.isEmpty) "" else ", "+
      tables.map{case (l, r) => "["+l.encJs+", "+r.encJs+"]"}.mkString(", "))+
      ")"
    }
    
    def apply(obj: JsExp,
    indexName: String, tables: (String, String)*): JsExp = new JsExp {
      def toJsCmd = "lift$.buildIndex("+obj.toJsCmd+", "+indexName.encJs+
      (if (tables.isEmpty) "" else ", "+
      tables.map{case (l, r) => "["+l.encJs+", "+r.encJs+"]"}.mkString(", "))+
      ")"
    }
  }
  
  protected trait MostLjFuncs {
     def funcName: String
     
     def apply(obj: String, func: String): JsExp = new JsExp {
      def toJsCmd = "lift$."+funcName+"("+obj+", "+func.encJs+")"
    }
    
    def apply(obj: JsExp, func: JsExp): JsExp = new JsExp {
      def toJsCmd = "lift$."+funcName+"("+obj.toJsCmd+", "+func.toJsCmd+")"
    }
  }
  
  object LjAlt {

    def apply(obj: String, func: String, alt: String): JsExp = new JsExp {
      def toJsCmd = "lift$.alt("+obj+", "+func.encJs+", "+alt.encJs+")"
    }
    
    def apply(obj: JsExp, func: JsExp, alt: String): JsExp = new JsExp {
      def toJsCmd = "lift$.alt("+obj.toJsCmd+", "+func.toJsCmd+", "+alt.encJs+")"
    }
    
    def apply(obj: JsExp, func: JsExp, alt: JsExp): JsExp = new JsExp {
      def toJsCmd = "lift$.alt("+obj.toJsCmd+", "+func.toJsCmd+", "+alt.toJsCmd+")"
    }
  }
  
  object LjMagicUpdate {
    def apply(obj: String, field: String, idField: String, toUpdate: JsExp): JsExp = new JsExp {
      def toJsCmd = "lift$.magicUpdate("+obj+", "+field.encJs+", "+idField.encJs+", "+toUpdate.toJsCmd+")"
    }
    
    def apply(obj: JsExp, field: String, idField: String, toUpdate: JsExp): JsExp = new JsExp {
      def toJsCmd = "lift$.magicUpdate("+obj.toJsCmd+", "+field.encJs+", "+idField.encJs+", "+toUpdate.toJsCmd+")"
    }
  }

  object LjForeach extends MostLjFuncs {
    def funcName: String = "foreach"   
  }
  
  object LjFilter extends MostLjFuncs {
    def funcName: String = "filter"   
  }
  
  object LjMap extends MostLjFuncs {
    def funcName: String = "map"   
  }
  
  object LjFold {
    def apply(what: JsExp, init: JsExp, func: String): JsExp = new JsExp {
      def toJsCmd = "lift$.fold("+what.toJsCmd+", "+init.toJsCmd+", "+func.encJs+")"
    }

    def apply(what: JsExp, init: JsExp, func: AnonFunc): JsExp = new JsExp {
      def toJsCmd = "lift$.fold("+what.toJsCmd+", "+init.toJsCmd+", "+func.toJsCmd+")"
    }
  }
  
  object LjFlatMap extends MostLjFuncs {
    def funcName: String = "flatMap"   
  }
  
  object LjSort extends MostLjFuncs {
    def funcName: String = "sort"   
    
    def apply(obj: String): JsExp = new JsExp {
      def toJsCmd = "lift$."+funcName+"("+obj+")"
    }
    
    def apply(obj: JsExp): JsExp = new JsExp {
      def toJsCmd = "lift$."+funcName+"("+obj.toJsCmd+")"
    }
  }
  
  /**
   * A String (JavaScript encoded)
   */
  case class Str(str: String) extends JsExp {
    def toJsCmd = str.encJs
  }
  
  /**
   * A JavaScript method that takes parameters
   */
  case class JsFunc(method: String, params: JsExp*) extends JsMethod {
    def toJsCmd = params.map(_.toJsCmd).mkString(method+"(", ", ", ")")
  }
  
  /**
   * Put any JavaScript expression you want in here and the result will be 
   * evaluated.
   */
  case class JsRaw(rawJsCmd: String) extends JsExp {
    def toJsCmd = rawJsCmd
  }
  
  case class JsVar(varName: String, andThen: String*) extends JsExp {
    def toJsCmd = varName + (if (andThen.isEmpty) "" 
			     else andThen.mkString(".", ".", ""))
  }
  
  /**
   * A value that can be retrieved from an expression
   */
  case class JsVal(valueName: String) extends JsMethod {
    def toJsCmd = valueName
  }
  
  case object Id extends JsMethod {
    def toJsCmd = "id"
  }

  case object Class extends JsMethod {
    def toJsCmd = "class"
  }

  case object Style extends JsMethod {
    def toJsCmd = "style"
  }

  case object Value extends JsMethod {
    def toJsCmd = "value"
  }
  
  case object JsFalse extends JsExp {
    def toJsCmd = "false"
  }
  
  case object JsNull extends JsExp {
    def toJsCmd = "null"
  }
  
  case object JsTrue extends JsExp {
    def toJsCmd = "true"
  }
  
  case object JqTabsSelected extends JsExp with JQueryRight {
    def toJsCmd = "tabsSelected()"
  }
  
  case object JqScrollToBottom extends JsExp with JQueryRight with JQueryLeft {
    def toJsCmd = "each(function(i) {this.scrollTop=this.scrollHeight;})"
  }
  
  object JqTabsClick {
    def apply(tab: JsExp): JsExp with JQueryRight with JQueryLeft = 
    new JsExp with JQueryRight with JQueryLeft {
      def toJsCmd = "tabsClick("+tab.toJsCmd+")"
    }
    
    def apply(tab: Int): JsExp with JQueryRight with JQueryLeft = 
    apply(Num(tab))
  }
  
  object JqTabs {
    def apply(in: JsExp): JsExp with JQueryRight with JQueryLeft = 
    new JsExp with JQueryRight with JQueryLeft {
      def toJsCmd = "tabs("+in.toJsCmd+")"
    }
    
    def apply(): JsExp with JQueryRight with JQueryLeft = 
    apply(JsRaw(""))
  }
  
  case class JqClick(exp: JsExp) extends JsExp with JQueryLeft with JQueryRight {
    def toJsCmd = "click(" + exp.toJsCmd + ")"
  }
  
  case class JqGetAttr(key: String) extends JsExp with JQueryRight with JQueryLeft {
    def toJsCmd = "attr(" + key.encJs + ")"
  }
  
  trait AnonFunc extends JsExp {
    def applied: JsExp = new JsExp {
      def toJsCmd = AnonFunc.this.toJsCmd + "()"
    }
  }
  
  object AnonFunc {
    def apply(in: JsCmd): AnonFunc = new JsExp with AnonFunc {
      def toJsCmd = "function() {"+in.toJsCmd+"}"
    }
    
    def apply(params: String, in: JsCmd): AnonFunc = new JsExp with AnonFunc {
      def toJsCmd = "function("+params+") {"+in.toJsCmd+"}"
    }
  }
  
  object JsObj {
    def apply(members: (String, JsExp)*): JsExp = 
      new JsExp {
	def toJsCmd = members.
	map{case (n, v) => n.encJs+": "+v.toJsCmd}.
	mkString("{", ", ", "}\n")
      }
  }
  
  /**
   * A JQuery query
   */
  case class Jq(query: JsExp) extends JsExp with JQueryLeft {
    override def toJsCmd = "jQuery("+query.toJsCmd+")"
  }
  
  /**
   * A JQuery query for an element based on the id of the element
   */
  case class JqId(id: JsExp) extends JsExp with JQueryLeft {
    override def toJsCmd = "jQuery('#'+"+id.toJsCmd+")"
  }
  
  case class JqAttr(key: String, value: JsExp) extends JsExp with JQueryRight with JQueryLeft {
    def toJsCmd = "attr("+key.encJs+", "+value.toJsCmd+")"
  }
  
  /**
   * Append content to a JQuery
   */
  case class JqAppend(content: NodeSeq) extends JsExp with JQueryRight with JQueryLeft {
    override def toJsCmd = "append("+fixHtml("inline", content)+")"
  }
  
  /**
   * AppendTo content to a JQuery
   */
  case class JqAppendTo(content: NodeSeq) extends JsExp with JQueryRight with JQueryLeft {
    override def toJsCmd = "appendTo("+fixHtml("inline", content)+")"
  }
  
  /**
   * Prepend content to a JQuery
   */
  case class JqPrepend(content: NodeSeq) extends JsExp with JQueryRight with JQueryLeft {
    override def toJsCmd = "prepend("+fixHtml("inline", content)+")"
  }
  
  /**
   * PrependTo content to a JQuery
   */
  case class JqPrependTo(content: NodeSeq) extends JsExp with JQueryRight with JQueryLeft {
    override def toJsCmd = "prependTo("+fixHtml("inline", content)+")"
  }

  /**
   * EmptyAfter will empty the node at the given uid and stick the given content behind it. Like
   * a cleaner innerHTML.
   */
  case class JqEmptyAfter(content: NodeSeq) extends JsExp with JQueryRight with JQueryLeft {
    override def toJsCmd = "empty().after("+fixHtml("inline", content)+")"
  }

  object JqHtml {
    def apply() = new JsExp with JQueryRight {
      def toJsCmd = "html()"
    }
    
    def apply(content: NodeSeq) = new JsExp with JQueryRight with JQueryLeft {
      def toJsCmd = "html("+fixHtml("inline", content)+")"
    }
  }
  
  object JqText {
    def apply() = new JsExp with JQueryRight {
      def toJsCmd = "text()"
    }
    
    def apply(content: String) = new JsExp with JQueryRight with JQueryLeft {
      def toJsCmd = "text("+content.encJs+")"
    }
  }

  /**
   * Serialize the jquery into a JSON array
   */
  case object JsonSerialize extends JsExp with JQueryRight {
    def toJsCmd = "serializeArray()"
  }
  
  case class JsLt(left: JsExp, right: JsExp) extends JsExp {
    def toJsCmd = left.toJsCmd + " < " + right.toJsCmd
  }

  case class JsGt(left: JsExp, right: JsExp) extends JsExp {
    def toJsCmd = left.toJsCmd + " > " + right.toJsCmd
  }
  
  case class JsEq(left: JsExp, right: JsExp) extends JsExp {
    def toJsCmd = left.toJsCmd + " == " + right.toJsCmd
  }
  
  case class JsNotEQ(left: JsExp, right: JsExp) extends JsExp {
    def toJsCmd = left.toJsCmd + " != " + right.toJsCmd
  }

  case class JsLtEq(left: JsExp, right: JsExp) extends JsExp {
    def toJsCmd = left.toJsCmd + " <= " + right.toJsCmd
  }
  
  case class JsGtEq(left: JsExp, right: JsExp) extends JsExp {
    def toJsCmd = left.toJsCmd + " >= " + right.toJsCmd
  }

}

trait JQueryRight {
  this: JsExp =>
  def toJsCmd: String
}

trait JQueryLeft {
  this: JsExp =>
  def >>(that: JQueryRight): JsExp = new JsExp {
    def toJsCmd = JQueryLeft.this.toJsCmd + "."+ that.toJsCmd
  }
  
  
  def >>(that: JQueryLeft with JQueryRight): JsExp with JQueryLeft =
    new JsExp with JQueryLeft {
      def toJsCmd = JQueryLeft.this.toJsCmd + "."+ that.toJsCmd
  }
}

trait HtmlFixer {
  def fixHtml(uid: String, content: NodeSeq): String = 
  AltXML.toXML(Group(S.session.map(s => s.fixHtml(s.processSurroundAndInclude("JS SetHTML id: "+uid, content))).openOr(content)),
  false, true).encJs
  
}

trait JsCmd extends HtmlFixer {
  def &(other: JsCmd): JsCmd = JsCmds.CmdPair(this, other)  
  def toJsCmd: String
}

object JsCmds {
  implicit def seqJsToJs(in: Seq[JsCmd]): JsCmd = in.foldLeft[JsCmd](Noop)(_ & _)
  
  object Script {
    def apply(script: JsCmd): NodeSeq = <script>{Unparsed("""
      // <![CDATA[
      """+script.toJsCmd+"""
      // ]]>""")
    }
    </script>
  }
  
  /**
  * Makes the parameter the selected HTML element on load of the page
  *
  * @param in the element that should have focus
  *
  * @return the element and a script that will give the element focus
  */
  object FocusOnLoad {
    def apply(in: Elem): NodeSeq = {
      val (elem, id) = findOrAddId(in)
      elem ++ Script(OnLoad(Run("document.getElementById("+id.encJs+").focus();")))
    }
  }
  
  case class SetValById(id: String, right: JsExp) extends JsCmd {
    def toJsCmd = "document.getElementById("+id.encJs+").value = "+
    right.toJsCmd+";"
  }
  
  case class SetExp(left: JsExp, right: JsExp) extends JsCmd {
    def toJsCmd = left.toJsCmd + " = " + right.toJsCmd + ";"
  }
  
  case class JsCrVar(name: String, right: JsExp) extends JsCmd {
    def toJsCmd = "var "+name + " = "+right.toJsCmd + ";"
  }
  
  case class SetElemById(id: String, right: JsExp, then: String*) extends JsCmd {
    def toJsCmd = "document.getElementById("+id.encJs+")"+ (
    if (then.isEmpty) "" else then.mkString(".", ".", "")
    ) + " = "+right.toJsCmd + ";"
  }
  
  implicit def jsExpToJsCmd(in: JsExp) = in.cmd
  
  
  case class OnLoad(cmd: JsCmd) extends JsCmd {
    def toJsCmd = "jQuery(document).ready(function() {"+cmd.toJsCmd+"});"
  }
  
  case class CmdPair(left: JsCmd, right: JsCmd) extends JsCmd {
    def toJsCmd = {
      val sb = new StringBuilder
      append(sb, this)
      sb.toString
    }
    
    private def append(sb: StringBuilder, cmd: JsCmd) {
      cmd match {
        case CmdPair(l, r) => append(sb, l)
        sb.append('\n')
        append(sb, r)
        
        case c => sb.append(c.toJsCmd)
      }
    }
  }
  
  /**
   * Append a NodeSeq to a node specified by uid using jQuery's append() method.
   */
  object AppendHtml {
    def apply(uid: String, content: NodeSeq): JsCmd =
      JE.JqId(JE.Str(uid)) >> JE.JqAppend(content)
  }

  /**
   * AppendTo a NodeSeq to a node specified by uid using jQuery's appendTo() method.
   */
  object AppendToHtml {
    def apply(uid: String, content: NodeSeq): JsCmd =
      JE.JqId(JE.Str(uid)) >> JE.JqAppendTo(content)
  }

  /**
   * Prepends a NodeSeq to a node specified by uid using jQuery's prepend() method.
   */
  object PrependHtml {
    def apply(uid: String, content: NodeSeq): JsCmd = 
      JE.JqId(JE.Str(uid)) >> JE.JqPrepend(content)
  }

  /**
   * Replaces the children of the node at {@code uid} with {@code content}
   */
  object EmptyAfter {
    def apply(uid: String, content: NodeSeq): JsCmd = 
      JE.JqId(JE.Str(uid)) >> JE.JqEmptyAfter(content)
  }

  /**
   * Prepends a NodeSeq to a node specified by uid using jQuery prependTo() method.
   */
  object PrependToHtml {
    def apply(uid: String, content: NodeSeq): JsCmd = 
      JE.JqId(JE.Str(uid)) >> JE.JqPrependTo(content)
  }
  
  
  case class SetHtml(uid: String, content: NodeSeq) extends JsCmd {
    def toJsCmd = {
      val ret = "try{jQuery("+("#"+uid).encJs+").each(function(i) {this.innerHTML = "+fixHtml(uid, content)+";});} catch (e) {}"
      ret
    }
  }
  
  trait HasTime {
    def time: Can[TimeSpan]
    def timeStr = time.map(_.millis.toString) openOr ""
  }
  
  case class After(time: TimeSpan, toDo: JsCmd) extends JsCmd {
    def toJsCmd = "setTimeout(function() {"+toDo.toJsCmd+"}, "+time.millis+");"
  }
  
  object Show {
    def apply(uid: String) = new Show(uid, Empty)
    def apply(uid: String, time: TimeSpan) = new Show(uid, Full(time))
  }
  
  class Show(val uid: String,val time: Can[TimeSpan]) extends JsCmd with HasTime {
    def toJsCmd = "try{jQuery("+("#"+uid).encJs+").show("+timeStr+");} catch (e) {}"
  }
  
  object Hide {
    def apply(uid: String) = new Hide(uid, Empty)
    def apply(uid: String, time: TimeSpan) = new Hide(uid, Full(time))    
  }
  
  class Hide(val uid: String, val time: Can[TimeSpan]) extends JsCmd with HasTime {
    def toJsCmd = "try{jQuery("+("#"+uid).encJs+").hide("+timeStr+");} catch (e) {}"
  }
  
  case class Alert(text: String) extends JsCmd {
    def toJsCmd = "alert("+text.encJs+");"
  }
  
  object ModalDialog {
    def apply(html: NodeSeq) = new ModalDialog(html, Empty)
    def apply(html: NodeSeq, width: String) = new ModalDialog(html, Full(width))    
  }
  
  class ModalDialog(html: NodeSeq, width: Can[String]) extends JsCmd {
    def toJsCmd = "jQuery.blockUI("+AltXML.toXML(Group(S.session.map(s => 
    s.fixHtml(s.processSurroundAndInclude("Modal Dialog", html))).openOr(html)), false, true).encJs+
    (width.map(w => ", { width: '"+w+"' }").openOr("")) + ");"
  }
  
  case class Run(text: String) extends JsCmd {
    def toJsCmd = text
  }
  
  case object _Noop extends JsCmd {
    def toJsCmd = ""
  }
  
  implicit def cmdToString(in: JsCmd): String = in.toJsCmd
  
  val Noop: JsCmd = _Noop
  
  case object Unblock extends JsCmd {
    def toJsCmd = "jQuery.unblockUI();"
  }
  
  case class JsTry(what: JsCmd, alert: Boolean) extends JsCmd {
    def toJsCmd = "try { "+what.toJsCmd+" } catch (e) {"+(if (alert) "alert(e);" else "")+"}"
  }
  
  case class SetValueAndFocus(id: String, value: String) extends JsCmd {
    def toJsCmd = "jQuery('#"+id+"').attr('value', "+value.encJs+"); document.getElementById("+id.encJs+").focus();"
  }
  
  case class RedirectTo(where: String) extends JsCmd {
    private val context = S.contextPath
    def toJsCmd = "window.location = "+S.encodeURL(context + where).encJs+";"
  }
  
  
  /**
  * Update a Select with new Options
  */
  case class ReplaceOptions(select: String, opts: List[(String, String)], dflt: Can[String]) extends JsCmd {
    def toJsCmd = """var x=document.getElementById("""+select.encJs+""");
    while (x.length > 0) {x.remove(0);}
    var y = null;
    """+
    opts.map{case (value, text) => "y=document.createElement('option'); "+
      "y.text = "+text.encJs+"; "+
      "y.value = "+value.encJs+"; "+
      (if (value == dflt) "y.selected = true; " else "") + "x.add(y, null); "
    }.mkString("\n")
  }
  
  case class DisplayMessage(where: String, msg: NodeSeq, duration: TimeSpan, fadeTime: TimeSpan) extends JsCmd {
    def toJsCmd = (Show(where) & SetHtml(where, msg) & After(duration, Hide(where, fadeTime))).toJsCmd
  }

  case object JsIf {
    def apply(condition: JsExp, body: JsExp):JsCmd = JE.JsRaw("if ( " + condition.toJsCmd  + " ) { " + body.toJsCmd + " }")  

    def apply(condition: JsExp, bodyTrue: JsExp, bodyFalse: JsExp) : JsCmd = 
      JE.JsRaw("if ( " + condition.toJsCmd  +" ) { " + bodyTrue.toJsCmd + " } else { " + bodyFalse.toJsCmd + " }")  
  }
  
  case class JsWhile(condition: JsExp, body: JsExp) extends JsCmd {
    def toJsCmd = "while ( " + condition.toJsCmd + " ) { " + body.toJsCmd + " }" 
  }

  case class JsWith(reference: String, body: JsExp) extends JsCmd {
    def toJsCmd = "with ( " + reference + " ) { " + body.toJsCmd + " }" 
  }

  case class JsDoWhile(body: JsExp, condition: JsExp) extends JsCmd {
    def toJsCmd = "do { " + body.toJsCmd + " } while ( " + condition.toJsCmd + " )" 
  }

  case class JsFor(initialExp: JsExp, condition: JsExp, incrementExp: JsExp, body: JsExp) extends JsCmd {
    def toJsCmd = "for ( " + initialExp.toJsCmd + "; " + 
                             condition.toJsCmd + "; " + 
                             incrementExp.toJsCmd + " ) { " + body.toJsCmd + " }" 
  }

  case class JsForIn(initialExp: JsExp, reference: String, body: JsCmd) extends JsCmd {
    def toJsCmd = "for ( " + initialExp.toJsCmd + " in " + reference+ ") { " + body.toJsCmd + " }" 
  }
  
  case object JsBreak extends JsCmd {
     def toJsCmd = "break"  
  }

  case object JsContinue extends JsCmd {
     def toJsCmd = "continue"  
  }

  case class JsReturn(in: JsExp) extends JsCmd {
     def toJsCmd = "return " + in.toJsCmd
  }

  
}



