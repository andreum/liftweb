package net.liftweb.proto

/*                                                *\
  (c) 2006-2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                */

import net.liftweb.mapper.{Mapper, MappedField, IndexedField}
import java.sql.{ResultSet, Types}
import java.lang.reflect.Method
import net.liftweb.util.Helpers._
import java.lang.Boolean
import java.util.Date

class MappedBoolean[T](val owner : Mapper[T]) extends MappedField[boolean, T] {
  private var data : Option[boolean] = Some(defaultValue)
  def defaultValue = false

  /**
   * Get the JDBC SQL Type for this field
   */
  def getTargetSQLType(field : String) = Types.BOOLEAN

  protected def i_get_! = data match {case None => false; case Some(v) => v}
  
  protected def i_set_!(value : boolean) : boolean = {
    if (data != None || value != data.get) {
      data = Some(value)
      this.dirty_?( true)
    }
    value
  }
  override def readPermission_? = true
  override def writePermission_? = true
  
  def convertToJDBCFriendly(value: boolean): Object = new Boolean(value)
      
      
  def getJDBCFriendly(field : String) = data match {case None => null; case _ => new Boolean(get)}

  def ::=(in : Any) : boolean = {
    in match {
      case b: boolean => this := b
      case (b: boolean) :: _ => this := b
      case Some(b: boolean) => this := b
      case None => this := false
      case (s: String) :: _ => this := toBoolean(s)
      case null => this := false
      case s: String => this := toBoolean(s)
      case o => this := toBoolean(o)
    }
  }

  protected def i_obscure_!(in : boolean) = false
  
  def buildSetActualValue(accessor : Method, inst : AnyRef, columnName : String) : (Mapper[T], AnyRef) => unit = {
    inst match {
      case null => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = Some(false)}}
      case _ => {(inst : Mapper[T], v : AnyRef) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = Some(toBoolean(v))}}
    }
  }
  
  def buildSetLongValue(accessor : Method, columnName : String) : (Mapper[T], long, boolean) => unit = {
    {(inst : Mapper[T], v: long, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = if (isNull) None else Some(v != 0L)}}
  }
  def buildSetStringValue(accessor : Method, columnName : String) : (Mapper[T], String) => unit  = {
    {(inst : Mapper[T], v: String ) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = if (v == null) None else Some(toBoolean(v))}}
  }
  def buildSetDateValue(accessor : Method, columnName : String) : (Mapper[T], Date) => unit   = {
    {(inst : Mapper[T], v: Date ) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = if (v == null) None else Some(true)}}
  }
  def buildSetBooleanValue(accessor : Method, columnName : String) : (Mapper[T], boolean, boolean) => unit   = {
    {(inst : Mapper[T], v: boolean, isNull: boolean ) => {val tv = getField(inst, accessor).asInstanceOf[MappedBoolean[T]]; tv.data = if (isNull) None else Some(v)}}
  }
}
