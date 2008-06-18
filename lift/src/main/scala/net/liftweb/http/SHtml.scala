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
package net.liftweb.http;
 
import S._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.http.js._
import scala.xml._

object SHtml {
  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   *
   * @return a button to put on your page
   */
  def ajaxButton(func: => JsCmd, text: String): Elem =
    <input type="button" value={text}/> % 
     ("onclick" -> ("jQuery.ajax( {url: '"+S.encodeURL(contextPath+"/"+LiftRules.ajaxPath)+"',  type: 'POST', timeout: 10000, cache: false, data: '"+
       mapFunc(() => func)+"=true', dataType: 'script'});"))

  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   *
   * @return a button to put on your page
   */
  def ajaxButton(text: String)(func: => JsCmd): Elem =
    <input type="button" value={text}/> % 
    ("onclick" -> ("jQuery.ajax( {url: '"+S.encodeURL(contextPath+"/"+LiftRules.ajaxPath)+"', timeout: 10000,  type: 'POST', cache: false, data: '"+
      mapFunc(() => func)+"=true', dataType: 'script'});"));
  
  /**
   * create an anchor tag around a body which will do an AJAX call and invoke the function
   *
   * @param func - the function to invoke when the link is clicked
   * @param body - the NodeSeq to wrap in the anchor tag
   */
  def a(func: () => JsCmd, body: NodeSeq): Elem = {
    val key = "F"+System.nanoTime+"_"+randomString(3)
    addFunctionMap(key, (a: List[String]) => func())
      (<lift:a key={key}>{body}</lift:a>)
  }
         
  /**
   * Create an anchor with a body and the function to be executed when the anchor is clicked
   */
  def a(body: NodeSeq)(func: => JsCmd): Elem = a(() => func, body)
         
  /**
   * Create an anchor that will run a JavaScript command when clicked
   */
  def a(body: NodeSeq, cmd: JsCmd): Elem = (<a href="javascript://" onclick={cmd.toJsCmd + "; return false;"}>{body}</a>)
         
  /**
   * Create a span that will run a JavaScript command when clicked
   */
  def span(body: NodeSeq, cmd: JsCmd): Elem = (<span onclick={cmd.toJsCmd}>{body}</span>)

  
  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param func -- the function to call when the data is sent
   *
   * @return the JavaScript that makes the call
   */
  def ajaxCall(jsCalcValue: String, func: String => JsCmd): String = ajaxCall_*(jsCalcValue, SFuncHolder(func))
     
  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param func -- the function to call when the data is sent
   *
   * @return the JavaScript that makes the call
   */
  private def ajaxCall_*(jsCalcValue: String, func: AFuncHolder): String =
    "jQuery.ajax( {url: '"+
      S.encodeURL(contextPath+"/"+LiftRules.ajaxPath)+"',  type: 'POST', timeout: 10000, cache: false, data: '"+mapFunc(func)+"='+encodeURIComponent("+jsCalcValue+"), dataType: 'script'});"
     
  def toggleKids(head: Elem, visible: Boolean, func: () => Any, kids: Elem): NodeSeq = {
    val funcName = mapFunc(func)
    val (nk, id) = findOrAddId(kids)
    val rnk = if (visible) nk else nk % ("style" -> "display: none") 
    val nh = head % ("onclick" -> ("jQuery('#"+id+"').toggle(); jQuery.ajax( {url: '"+
      S.encodeURL(contextPath+"/"+LiftRules.ajaxPath)+"', type: 'POST', cache: false, data: '"+funcName+"=true', dataType: 'script'});"))
    nh ++ rnk
  }

  /**
   * Create a JSON text widget that makes a JSON call on blur or "return".
   * Note that this is not "Stateful" and will be moved out of S at some
   * point.
   *
   * @param value - the initial value of the text field
   * @param json - takes a JsExp which describes how to recover the
   * value of the text field and returns a JsExp containing the thing
   * to execute on blur/return
   *
   * @return a text field
   */
  def jsonText(value: String, json: JsExp => JsCmd): Elem = {
    (<input type="text" value={value}/>) %
    ("onkeypress" -> """var e = event ; var char = ''; if (e && e.which) {char = e.which;} else {char = e.keyCode;}; if (char == 13) {this.blur(); return false;} else {return true;};""") %
    ("onblur" -> (json(JE.JsRaw("this.value")).toJsCmd))
  }
       
  def ajaxText(value: String, func: String => JsCmd): Elem = ajaxText_*(value, SFuncHolder(func))
       
  private def ajaxText_*(value: String, func: AFuncHolder): Elem = {
    val funcName = mapFunc(func) 
      (<input type="text" value={value}/>) %
        ("onkeypress" -> """var e = event ; var char = ''; if (e && e.which) {char = e.which;} else {char = e.keyCode;}; if (char == 13) {this.blur(); return false;} else {return true;};""") %
        ("onblur" -> ("jQuery.ajax( {url: '"+S.encodeURL(contextPath+"/"+LiftRules.ajaxPath)+"', timeout: 10000,  type: 'POST', cache: false, data: '"+funcName+"='+encodeURIComponent(this.value), dataType: 'script'});"))
  }
       
  def ajaxCheckbox(value: Boolean, func: String => JsCmd): Elem = ajaxCheckbox_*(value, SFuncHolder(func))
       
  private def ajaxCheckbox_*(value: Boolean, func: AFuncHolder): Elem = {
    val funcName = mapFunc(func)
      (<input type="checkbox"/>) % checked(value) %
        ("onclick" -> ("jQuery.ajax( {url: '"+S.encodeURL(contextPath+"/"+LiftRules.ajaxPath)+"', timeout: 10000,  type: 'POST', cache: false, data: '"+funcName+"='+this.checked, dataType: 'script'});"))        
  }
       
  def ajaxSelect(opts: List[(String, String)], deflt: Can[String], func: String => JsCmd): Elem = ajaxSelect_*(opts, deflt, SFuncHolder(func))
       
  private def ajaxSelect_*(opts: List[(String, String)],deflt: Can[String], func: AFuncHolder): Elem = {
    val vals = opts.map(_._1)
    val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)
    val funcName = mapFunc(testFunc)
         
    (<select>{
       opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
    }</select>) % ("onchange" -> ("jQuery.ajax( {url: '"+S.encodeURL(contextPath+"/"+LiftRules.ajaxPath)+"', timeout: 10000,  type: 'POST', cache: false, data: '"+funcName+"='+this.options[this.selectedIndex].value, dataType: 'script'});"))
  }
       
  def ajaxInvoke(func: () => JsCmd): String = "jQuery.ajax( {url: '"+S.encodeURL(contextPath+"/"+LiftRules.ajaxPath)+"',  type: 'POST', cache: false, timeout: 10000, data: '"+
     mapFunc(NFuncHolder(func))+"=true', dataType: 'script'});"
       
  /**
   *  Build a swappable visual element.  If the shown element is clicked on, it turns into the hidden element and when
   * the hidden element blurs, it swaps into the shown element.
   */
  def swappable(shown: Elem, hidden: Elem): Elem = {
    val (rs, sid) = findOrAddId(shown)
    val (rh, hid) = findOrAddId(hidden)
    (<span>{rs % ("onclick" -> ("jQuery('#"+sid+"').hide(); jQuery('#"+hid+"').show().each(function(i) {var t = this; setTimeout(function() { t.focus(); }, 200);}); return false;"))}{
      dealWithBlur(rh % ("style" -> "display: none"), ("jQuery('#"+sid+"').show(); jQuery('#"+hid+"').hide();"))}</span>)
  }
       
  def swappable(shown: Elem, hidden: String => Elem): Elem = {
    val (rs, sid) = findOrAddId(shown)
    val hid = "S"+randomString(10)
    val rh = <span id={hid}>{hidden("jQuery('#"+sid+"').show(); jQuery('#"+hid+"').hide();")}</span>
      (<span>{rs % ("onclick" -> ("jQuery('#"+sid+"').hide(); jQuery('#"+hid+"').show(); return false;"))}{
         (rh % ("style" -> "display: none"))}</span>)      
  }
       
  private def dealWithBlur(elem: Elem, blurCmd: String): Elem = {
   (elem \ "@onblur").toList match {
      case Nil => elem % ("onblur" -> blurCmd)
      case x :: xs => val attrs = elem.attributes.filter(_.key != "onblur")
         Elem(elem.prefix, elem.label, new UnprefixedAttribute("onblur", Text(blurCmd + x.text), attrs), elem.scope, elem.child :_*)
     }
   }
  
  
  /**
   * create an anchor tag around a body 
   *
   * @param func - the function to invoke when the link is clicked
   * @param body - the NodeSeq to wrap in the anchor tag
   */
  def link(to: String, func: () => Any, body: NodeSeq): Elem = {
    val key = mapFunc((a: List[String]) => {func(); true})
      (<a href={to+"?"+key+"=_"}>{body}</a>)
  }
   
  private def makeFormElement(name: String, func: AFuncHolder): Elem = (<input type={name} name={mapFunc(func)}/>)
  
  def text_*(value: String, func: AFuncHolder): Elem = makeFormElement("text", func) % new UnprefixedAttribute("value", Text(value), Null)
  def password_*(value: String, func: AFuncHolder): Elem = makeFormElement("password", func) % new UnprefixedAttribute("value", Text(value), Null)
  def hidden_*(func: AFuncHolder): Elem = makeFormElement("hidden", func) % ("value" -> "true")
  def submit_*(value: String, func: AFuncHolder): Elem = makeFormElement("submit", func) % new UnprefixedAttribute("value", Text(value), Null)
  def text(value: String, func: String => Any): Elem = makeFormElement("text", SFuncHolder(func)) % new UnprefixedAttribute("value", Text(value), Null)
  def password(value: String, func: String => Any): Elem = makeFormElement("password", SFuncHolder(func)) % new UnprefixedAttribute("value", Text(value), Null)
  def hidden(func: String => Any): Elem = makeFormElement("hidden", SFuncHolder(func)) % ("value" -> "true")
  def submit(value: String, func: String => Any): Elem = makeFormElement("submit", SFuncHolder(func)) % new UnprefixedAttribute("value", Text(value), Null)
   
  def ajaxForm(body: NodeSeq) = (<lift:form>{body}</lift:form>)
  def ajaxForm(onSubmit: JsCmd, body: NodeSeq) = (<lift:form onsubmit={onSubmit.toJsCmd}>{body}</lift:form>)
  def ajaxForm(body: NodeSeq, onSubmit: JsCmd) = (<lift:form onsubmit={onSubmit.toJsCmd}>{body}</lift:form>)
    
  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission
   *
   * @param opts -- the options.  A list of value and text pairs (value, text to display)
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
   def select(opts: List[(String, String)], deflt: Can[String], func: String => Any): Elem =
   select_*(opts, deflt, SFuncHolder(func))
   
     /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission
   *
   * @param opts -- the options.  A list of value and text pairs (value, text to display)
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
   def selectObj[T](opts: List[(T, String)], deflt: Can[T], func: T => Any): Elem = {
     val secure = opts.map{case (obj, txt) => (obj, randomString(20), txt)}
     val revDflt = deflt.flatMap(o => secure.filter(_._1 == o) match {
       case x :: _ => Full(x._2)
       case _ => Empty
     })
     val toPass = secure.map{case (obj, ran, txt) => (ran, txt)}
     def process(in: String): Any = secure.filter(_._2 == in) match {
       case Nil =>
       case x :: _ => func(x._1)
     }
     select_*(toPass, revDflt, SFuncHolder(process))
   }
     
  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def select_*(opts: List[(String, String)],deflt: Can[String], func: AFuncHolder): Elem = {
    val vals = opts.map(_._1)
    val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)
       
    (<select name={mapFunc(testFunc)}>{
      opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
    }</select>)
  }
     
  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission.  No check is made to see if the resulting value was in the original list.
   * For use with DHTML form updating.
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def untrustedSelect(opts: List[(String, String)], deflt: Can[String], func: String => Any): Elem = untrustedSelect_*(opts, deflt, SFuncHolder(func))
     
  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission.  No check is made to see if the resulting value was in the original list.
   * For use with DHTML form updating.
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def untrustedSelect_*(opts: List[(String, String)],deflt: Can[String], func: AFuncHolder): Elem = {
    (<select name={mapFunc(func)}>{
      opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
    }</select>)
  }
     
     
  private def selected(in: Boolean) = if (in) new UnprefixedAttribute("selected", "true", Null) else Null
     
  def multiSelect(opts: List[(String, String)], deflt: List[String], func: String => Any): Elem = multiSelect_*(opts, deflt, SFuncHolder(func))
     
  def multiSelect_*(opts: List[(String, String)], deflt: List[String],func: AFuncHolder): Elem = (<select multiple="true" name={mapFunc(func)}>{
    opts.flatMap(o => (<option value={o._1}>{o._2}</option>) % selected(deflt.contains(o._1)))
  }</select>)
     
     
  def textarea(value: String, func: String => Any): Elem = textarea_*(value, SFuncHolder(func))
     
  def textarea_*(value: String, func: AFuncHolder): Elem = (<textarea name={mapFunc(func)}>{value}</textarea>) 
     
  def radio(opts: List[String], deflt: Can[String], func: String => Any): ChoiceHolder[String] =
    radio_*(opts, deflt, SFuncHolder(func))
     
  def radio_*(opts: List[String], deflt: Can[String], func: AFuncHolder): ChoiceHolder[String] = {
    val name = mapFunc(func)
    val itemList = opts.map(v => ChoiceItem(v, (<input type="radio" name={name} value={v}/>) % 
      checked(deflt.filter((s: String) => s == v).isDefined)))
      ChoiceHolder(itemList)
  }
     
  def fileUpload(func: FileParamHolder => Any): Elem = <input type="file" name={mapFunc(BinFuncHolder(func))} />
     
  case class ChoiceItem[T](key: T, xhtml: NodeSeq)
     
  case class ChoiceHolder[T](items: List[ChoiceItem[T]]) {
    def apply(in: T) = items.filter(_.key == in).head.xhtml
    def apply(in: Int) = items(in).xhtml
    def map[A](f: ChoiceItem[T] => A) = items.map(f)
    def flatMap[A](f: ChoiceItem[T] => Iterable[A]) = items.flatMap(f)
    def filter(f: ChoiceItem[T] => Boolean) = items.filter(f)
    def toForm: NodeSeq = flatMap(c => (<span>{c.xhtml}&nbsp;{c.key.toString}<br /></span>))
  }
     
  private def checked(in: Boolean) = if (in) new UnprefixedAttribute("checked", "checked", Null) else Null 
  private def setId(in: Can[String]) = in match { case Full(id) => new UnprefixedAttribute("id", Text(id), Null); case _ => Null}
     
  def checkbox[T](possible: List[T], actual: List[T], func: List[T] => Any): ChoiceHolder[T] = {
    val len = possible.length
    val name = mapFunc(LFuncHolder( (strl: List[String]) => {func(strl.map(toInt(_)).filter(x =>x >= 0 && x < len).map(possible(_))); true}))
       
    ChoiceHolder(possible.zipWithIndex.map(p => 
    ChoiceItem(p._1, (<input type="checkbox" name={name} value={p._2.toString}/>) % checked(actual.contains(p._1)) ++ (if (p._2 == 0) (<input type="hidden" name={name} value="-1"/>) else Nil))))
  }
     
  /**
   * Defines a new checkbox set to {@code value} and running {@code func} when the 
   * checkbox is submitted.
   */
  def checkbox(value: Boolean, func: Boolean => Any): NodeSeq = {
    checkbox_id(value, func, Empty)
  }
     
  /**
   * Defines a new checkbox set to {@code value} and running {@code func} when the
   * checkbox is submitted. Has an id of {@code id}.
   */
  def checkbox_id(value: Boolean, func: Boolean => Any, id: Can[String]): NodeSeq = {
    def from(f: Boolean => Any): List[String] => Boolean = (in: List[String]) => {
      f(in.exists(toBoolean(_)))
        true
    }
    checkbox_*(value, LFuncHolder(from(func)), id)
  }
     
  def checkbox_*(value: Boolean, func: AFuncHolder, id: Can[String]): NodeSeq = {
    val name = mapFunc(func)
    (<input type="hidden" name={name} value="false"/>) ++
      ((<input type="checkbox" name={name} value="true" />) % checked(value) % setId(id))
  }


}
