/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb.record

import net.liftweb._
import util._
import scala.xml._
import net.liftweb.http.js.{JsExp, JE}
import net.liftweb.http.{FieldError, SHtml}
import net.liftweb.mapper.{Safe, KeyObfuscator}
import field._

trait Record[MyType <: Record[MyType]] {
  self: MyType =>

  /**
   * A unique identifier for this record... used for access control
   */
  private val secure_# = Safe.next

  /**
   * The meta record (the object that contains the meta result for this type)
   */
  def meta: MetaRecord[MyType]

  /**
   * Is it safe to make changes to the record (or should we check access control?)
   */
  final def safe_? : Boolean = {
    Safe.safe_?(secure_#)
  }

  def runSafe[T](f : => T) : T = {
    Safe.runSafe(secure_#)(f)
  }

  /**
   * Returns the HTML representation ofthis Record
   */
  def asHtml: NodeSeq = {
    meta.asHtml(this)
  }

  /**
   * If the instance calculates any additional
   * fields for JSON object, put the calculated fields
   * here
   */
  def suplementalJs(ob: Can[KeyObfuscator]): List[(String, JsExp)] = Nil

  /**
   * Validates this Record by calling validators for each field
   *
   * @return a List of FieldError. If this list is empty you can assume that record was validated successfully
   */
  def validate : List[FieldError] = {
    runSafe {
      meta.validate(this)
    }
  }

  /**
   * Retuns the JavaScript expression for this Record
   *
   * @return a JsExp
   */
  def asJs: JsExp = {
    meta.asJs(this)
  }

  /**
   * Present the model as a form and execute the function on submission of the form
   *
   * @param button - If it's Full, put a submit button on the form with the value of the parameter
   * @param f - the function to execute on form submission
   *
   * @return the form
   */
  def toForm(button: Can[String])(f: MyType => Unit): NodeSeq = {
    meta.toForm(this) ++
    (SHtml.hidden(() => f(this))) ++
    ((button.map(b => (<input type="submit" value={b}/>)) openOr scala.xml.Text("")))
  }

  /**
   * Present the model as a form and execute the function on submission of the form.
   * The form is based on the given template. This template is any given XHtml that contains
   * three nodes acting as placeholders such as:
   *
   * <pre>
   *
   * &lt;lift:field_label name="firstName"/&gt; - the label for firstName field will be rendered here
   * &lt;lift:field name="firstName"/&gt; - the firstName field will be rendered here (typically an input field)
   * &lt;lift:field_msg name="firstName"/&gt; - the <lift:msg> will be rendered here hafing the id given by
   *                                             uniqueFieldId of the firstName field.
   *
   *
   * Example.
   *
   * Having:
   *
   * class MyRecord extends Record[MyRecord] {
   *
   * 	def meta = MyRecordMeta
   *
   * 	object firstName extends StringField(this, "John")
   *
   * }
   *
   * object MyRecordMeta extends MyRecord with MetaRecord[MyRecord] {
   *  override def mutable_? = false
   * }
   *
   * ...
   *
   * val rec = MyRecordMeta.createRecord.firstName("McLoud")
   *
   * val template =
   * &lt;div&gt;
   * 	&lt;div&gt;
   * 		&lt;div&gt;&lt;lift:field_label name="firstName"/&gt;&lt;/div&gt;
   * 		&lt;div&gt;&lt;lift:field name="firstName"/&gt;&lt;/div&gt;
   * 		&lt;div&gt;&lt;lift:field_msg name="firstName"/&gt;&lt;/div&gt;
   * 	&lt;/div&gt;
   * &lt;/div&gt;
   *
   * rec.toForm(template){(r:MyRecord) => println(r)});
   *
   * </pre>
   *
   * @param template - the XHtml template for rendering the form
   * @param f - the function to execute on form submission
   *
   * @return the form
   */
  def toForm(template: NodeSeq)(f: MyType => Unit): NodeSeq = meta.toForm(this, template) ++ (SHtml.hidden(() => f(this)))

  /**
   * Find the field by name
   * @param fieldName -- the name of the field to find
   *
   * @return Can[MappedField]
   */
  def fieldByName[T](fieldName: String): Can[Field[T, MyType]] = meta.fieldByName[T](fieldName, this)
}

trait ExpandoRecord[MyType <: Record[MyType] with ExpandoRecord[MyType]] {
  self: MyType =>

  /**
   * If there's a field in this record that defines the locale, return it
   */
  def localeField: Can[LocaleField[MyType]] = Empty

  def timeZoneField: Can[TimeZoneField[MyType]] = Empty

  def countryField: Can[CountryField[MyType]] = Empty
}


trait KeyedRecord[MyType <: KeyedRecord[MyType, KeyType] with Record[MyType], KeyType] {
  self: MyType =>

  def primaryKey: KeyField[KeyType, MyType]

  def comparePrimaryKeys(other: MyType) = primaryKey === other.primaryKey
}
