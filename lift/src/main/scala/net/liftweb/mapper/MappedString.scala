package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import net.liftweb.util.{FatLazy, Can, Full, Empty, Failure}
import java.util.Date
import java.util.regex._
import scala.xml.{NodeSeq, Text}
import net.liftweb.http.{S, FieldError}
import net.liftweb.http.js._
import S._

/*
trait NiceLength[MyType <: MappedString[_]] {self: MyType =>
/**
   * A list of functions that transform the value before it is set.  The transformations
   * are also applied before the value is used in a query.  Typical applications
   * of this are trimming and/or toLowerCase-ing strings
   */
 override protected def setFilter = crop _ :: super.setFilter	
}*/
  
/**
  * Just like MappedString, except it's defaultValue is "" and the length is auto-cropped to
  * fit in the column
  */
class MappedPoliteString[T <: Mapper[T]](towner: T, theMaxLen: Int) extends MappedString[T](towner, theMaxLen) {
  override def defaultValue = ""
  override protected def setFilter = crop _ :: super.setFilter  
}

class MappedString[T<:Mapper[T]](val fieldOwner: T,val maxLen: Int) extends MappedField[String, T] {
  private val data: FatLazy[String] =  FatLazy(defaultValue) // defaultValue
  private val orgData: FatLazy[String] =  FatLazy(defaultValue) // defaultValue
  
  def dbFieldClass = classOf[String]

  final def crop(in: String): String = in.substring(0, Math.min(in.length, maxLen))
  
  final def removeRegExChars(regEx: String)(in: String): String = in.replaceAll(regEx, "")
  
  final def toLower(in: String): String = in match {
    case null => null
    case s => s.toLowerCase
  }
  final def toUpper(in: String): String = in match {
    case null => null
    case s => s.toUpperCase
  }

  final def trim(in: String): String = in match {
    case null => null
    case s => s.trim
  }
  
  final def notNull(in: String): String = in match {
    case null => ""
    case s => s
  }

  
  protected def real_i_set_!(value : String) : String = {
    if (!data.defined_? || value != data.get) {
      data() = value
      this.dirty_?( true)
    }
    data.get
  }
  
  /**
  * Get the JDBC SQL Type for this field
  */
  def targetSQLType = Types.VARCHAR
  
  def defaultValue = ""

  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_is_! = data.get
  protected def i_was_! = orgData.get

  /**
     * Called after the field is saved to the database
     */
  override protected[mapper] def doneWithSave() {
    orgData.setFrom(data)
  }
  
  override def _toForm: Can[NodeSeq] = 
    if (readOnly_?) {
      Full(<input type='text' maxlength={maxLen.toString} 
	    name={S.mapFunc({s: List[String] => this.setFromAny(s)})} 
	    value={is match {case null => "" case s => s.toString}} readonly="readonly" />)  
    }
    else {
      Full(<input type='text' maxlength={maxLen.toString} 
	    name={S.mapFunc({s: List[String] => this.setFromAny(s)})} 
	    value={is match {case null => "" case s => s.toString}}  />)  
    }


  protected def i_obscure_!(in : String) : String = {
    ""
  }
  
  override def setFromAny(in: Any): String = {
    in match {
      case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny)(0)
      case (s: String) :: _ => this.set(s)
      case null => this.set(null)
      case s: String => this.set(s)
      case Some(s: String) => this.set(s)
      case Full(s: String) => this.set(s)
      case None | Empty | Failure(_, _, _) => this.set(null)
      case o => this.set(o.toString)
    }
  }
  
  
  def apply(ov: Can[String]): T = {
    ov.foreach(v => this.set(v))
    fieldOwner
  }

  def asJsExp: JsExp = JE.Str(is)
  
  def apply(ov: String): T = apply(Full(ov))
  
  def jdbcFriendly(field : String): String = data.get
  
  def real_convertToJDBCFriendly(value: String): Object = value
  
  private def wholeSet(in: String) {
    this.data() = in
    this.orgData() = in
  }
  
  def buildSetActualValue(accessor: Method, inst: AnyRef, columnName: String): (T, AnyRef) => Unit =
    (inst, v) => doField(inst, accessor, {case f: MappedString[T] => f.wholeSet(if (v eq null) null else v.toString)})
  
  def buildSetLongValue(accessor: Method, columnName: String): (T, Long, Boolean) => Unit =
    (inst, v, isNull) => doField(inst, accessor, {case f: MappedString[T] => f.wholeSet(if (isNull) null else v.toString)})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
    (inst, v) => doField(inst, accessor, {case f: MappedString[T] => f.wholeSet(if (v eq null) null else v)})

  def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit =
    (inst, v) => doField(inst, accessor, {case f: MappedString[T] => f.wholeSet(if (v eq null) null else v.toString)})

  def buildSetBooleanValue(accessor: Method, columnName: String): (T, Boolean, Boolean) => Unit = 
    (inst, v, isNull) => doField(inst, accessor, {case f: MappedString[T] => f.wholeSet(if (isNull) null else v.toString)})
  
  /**
   * A validation helper.  Make sure the string is at least a particular
   * length and generate a validation issue if not
   */
  def valMinLen(len: int, msg: String)(value: String): List[FieldError] = 
    if ((value eq null) || value.length < len) List(FieldError(this, Text(msg)))
    else Nil

  /**
   * A validation helper.  Make sure the string is no more than a particular
   * length and generate a validation issue if not
   */
  def valMaxLen(len: int, msg: String)(value: String): List[FieldError] = 
    if ((value ne null) && value.length > len) List(FieldError(this, Text(msg)))
    else Nil

  /**
   * Make sure that the field is unique in the database
   */
  def valUnique(msg: String)(value: String): List[FieldError] =
    fieldOwner.getSingleton.findAll(By(this,value)).
      filter(!_.comparePrimaryKeys(this.fieldOwner)).
      map(x =>FieldError(this, Text(msg)))

  /**
   * Make sure the field matches a regular expression
   */
  def valRegex(pat: Pattern, msg: String)(value: String): List[FieldError] = pat.matcher(value).matches match {
    case true => Nil
    case false => List(FieldError(this, Text(msg)))
  }

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName+" VARCHAR("+maxLen+")"
  
}
