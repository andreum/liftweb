package net.liftweb.mapper

import java.sql.{Connection, ResultSet, Statement, PreparedStatement, Types, ResultSetMetaData}
import javax.sql.{ DataSource}
import net.liftweb.util.{FatLazy,Full,Can}
import java.lang.reflect.Method
import net.liftweb.http.js._
import java.util.Date

class MappedClob[T<:Mapper[T]](val fieldOwner: T) extends MappedField[String, T] {
    private val data : FatLazy[String] =  FatLazy(defaultValue)
  private val orgData: FatLazy[String] = FatLazy(defaultValue)
  private var clob:Option[Object] = None
  protected def real_i_set_!(value: String): String = {
    data() = value
    this.dirty_?( true)
    value
  }

  def dbFieldClass = classOf[String]
  
  /**
  * Get the JDBC SQL Type for this field
  */
  //  def getTargetSQLType(field : String) = Types.BINARY
  def targetSQLType = Types.CLOB
  
  def defaultValue: String = null
  override def writePermission_? = true
  override def readPermission_? = true
  
  protected def i_is_! = data.get
  
  protected def i_was_! = orgData.get
  
  protected[mapper] def doneWithSave() {orgData.setFrom(data)}
  
  protected def i_obscure_!(in: String): String = ""
  
  def apply(ov: Can[String]): T = {
    ov.foreach(v => this.set(v))
    fieldOwner
  }

  def asJsExp: JsExp = JE.Str(is)
  
  def apply(ov: String): T = apply(Full(ov))
  
  override def setFromAny(f: Any): String =
  this.set(f match {
    case null => null
    case s: String => s
    case other => other.toString
  })
  
  def jdbcFriendly(field : String): Object = real_convertToJDBCFriendly(data.get)
  
  
  def real_convertToJDBCFriendly(value: String): Object = value match {
    case null => null
    case s => 
      DB.use(fieldOwner.connectionIdentifier) { conn => 
        conn.driverType.createClobJdbcObject(conn.connection,value) match {
           // free CLOB after commit - ideally after the DML (requires decorated statement)
           case (clob,Some(free)) => DB.appendPostFunc(fieldOwner.connectionIdentifier, free); clob
           case (value,_) => value // auto-allocated object
         }
     }
  }
  
  def buildSetActualValue(accessor: Method, inst: AnyRef, columnName: String): (T, AnyRef) => Unit = 
  (inst, v) => doField(inst, accessor, {case f: MappedClob[T] =>
    val toSet = v match {
      case null => null
      case ba: java.sql.Clob => ba.getSubString(1,ba.length.asInstanceOf[Int])
      case other => other.toString
    }
    f.data() = toSet
    f.orgData() = toSet
  })
  
  def buildSetLongValue(accessor : Method, columnName : String): (T, Long, Boolean) => Unit = null
  def buildSetStringValue(accessor : Method, columnName : String): (T, String) => Unit  = (inst, v) => doField(inst, accessor, {case f: MappedClob[T] =>
    val toSet = v match {
      case null => null
      case other => other
    }
    f.data() = toSet
    f.orgData() = toSet
  })
  def buildSetDateValue(accessor : Method, columnName : String): (T, Date) => Unit = null 
  def buildSetBooleanValue(accessor : Method, columnName : String): (T, Boolean, Boolean) => Unit = null
  
  /**
  * Given the driver type, return the string required to create the column in the database
  */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.binaryColumnType
}
