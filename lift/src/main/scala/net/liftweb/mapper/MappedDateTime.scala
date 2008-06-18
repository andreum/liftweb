package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.sql.{ResultSet, Types}
import java.util.Date
import java.lang.reflect.Method
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.http.js._

class MappedDateTime[T<:Mapper[T]](val fieldOwner: T) extends MappedField[Date, T] {
  private val data = FatLazy(defaultValue)
  private val orgData = FatLazy(defaultValue)
  
  protected def real_i_set_!(value: Date): Date = {
    if (value != data.get) {
      data() = value
      this.dirty_?( true)
    }
    data.get
  }
  
  def dbFieldClass = classOf[Date]

  
  def toLong: Long = is match {
    case null => 0L
    case d: Date => d.getTime / 1000L
  }
  
  def asJsExp = JE.Num(toLong)
  
  /**
   * Get the JDBC SQL Type for this field
   */
  def targetSQLType = Types.TIMESTAMP
  
  def defaultValue: Date = null
  // private val defaultValue_i = new Date

  override def writePermission_? = true
  override def readPermission_? = true

  protected def i_is_! = data.get
  protected def i_was_! = orgData.get
  protected[mapper] def doneWithSave() {orgData.setFrom(data)}

  protected def i_obscure_!(in : Date) : Date = {
    new Date(0L)
  }
  
  override def setFromAny(f : Any): Date = toDate(f).map(d => this.set(d)).openOr(this.is)
  
  def jdbcFriendly(field : String) : Object = is match {
    case null => null
    case d => new java.sql.Date(d.getTime)
  }
  
  def real_convertToJDBCFriendly(value: Date): Object = if (value == null) null else new java.sql.Date(value.getTime)
  
  private def st(in: Can[Date]): Unit = 
    in match {
      case Full(d) => data.set(d); orgData.set(d)
      case _ => data.set(null); orgData.set(null)
    }
  
  def buildSetActualValue(accessor: Method, v: AnyRef, columnName: String): (T, AnyRef) => Unit =
    (inst, v) => doField(inst, accessor, {case f: MappedDateTime[T] => f.st(toDate(v))})
  
  def buildSetLongValue(accessor: Method, columnName: String): (T, Long, Boolean) => Unit = 
    (inst, v, isNull) => doField(inst, accessor, {case f: MappedDateTime[T] => f.st(if (isNull) Empty else Full(new Date(v)))})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit = 
    (inst, v) => doField(inst, accessor, {case f: MappedDateTime[T] => f.st(toDate(v))})  

  def buildSetDateValue(accessor: Method, columnName: String): (T, Date) => Unit = 
    (inst, v) => doField(inst, accessor, {case f: MappedDateTime[T] => f.st(Full(v))})   

  def buildSetBooleanValue(accessor: Method, columnName: String): (T, Boolean, Boolean) => Unit = 
    (inst, v, isNull) => doField(inst, accessor, {case f: MappedDateTime[T] => f.st(Empty)})  

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.dateTimeColumnType
  
  def inFuture_? = data.get match {
    case null => false
    case d => d.getTime > millis
  }
  def inPast_? = data.get match {
    case null => false
    case d => d.getTime < millis
  }
}
