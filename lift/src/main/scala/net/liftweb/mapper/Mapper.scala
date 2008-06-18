package net.liftweb.mapper

/*                                                *\
(c) 2006 WorldWide Conferencing, LLC
Distributed under an Apache License
http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.collection.mutable._
import java.lang.reflect.Method
import java.sql.{ResultSet, Types}
import scala.xml.{Elem, Node, NodeSeq}
import net.liftweb.http.{S, FieldError}
import S._
import net.liftweb.http.js._
import net.liftweb.util.{Can, Empty, Full, Failure}

@serializable
trait Mapper[A<:Mapper[A]] { self: A =>
  private val secure_# = Safe.next
  private var was_deleted_? = false
  private var dbConnectionIdentifier:Can[ConnectionIdentifier] = Empty
  private[mapper] var addedPostCommit = false
  
  def getSingleton : MetaMapper[A];
  final def safe_? : boolean = {
    Safe.safe_?(secure_#)
  }
  
  implicit def thisToMappee(in: Mapper[A]): A = this.asInstanceOf[A]
  
  def runSafe[T](f : => T) : T = {
    Safe.runSafe(secure_#)(f)
  }
  
  def connectionIdentifier(id: ConnectionIdentifier): A = {
    if (id != getSingleton.dbDefaultConnectionIdentifier || dbConnectionIdentifier.isDefined) dbConnectionIdentifier = Full(id)
    thisToMappee(this)
  }
  
  def connectionIdentifier = dbConnectionIdentifier openOr calcDbId
  
  def dbCalculateConnectionIdentifier: PartialFunction[A, ConnectionIdentifier] = Map.empty
  
  private def calcDbId = if (dbCalculateConnectionIdentifier.isDefinedAt(this)) dbCalculateConnectionIdentifier(this)
  else getSingleton.dbDefaultConnectionIdentifier
  
  /**
  * Append a function to perform after the commit happens
  * @param func - the function to perform after the commit happens
  */
  def doPostCommit(func: () => Unit) {
    DB.appendPostFunc(connectionIdentifier, func)
  }
  
  /**
  * Save the instance and return the instance
  */
  def saveMe(): A = {
    this.save
    this
  }
  
  def save(): boolean = {
    runSafe {
      getSingleton.save(this)
    }
  }
  
  def htmlLine : NodeSeq = {
    getSingleton.doHtmlLine(this)
  }
  
  def asHtml : NodeSeq = {
    getSingleton.asHtml(this)
  }
  
  /**
  * If the instance calculates any additional
  * fields for JSON object, put the calculated fields
  * here
  */
  def suplementalJs(ob: Can[KeyObfuscator]): List[(String, JsExp)] = Nil
  
  def validate : List[FieldError] = {
    runSafe {
      getSingleton.validate(this)
    }
  }
  
  def asJs: JsExp = {
    getSingleton.asJs(this)
  }
  
  /*
  def toForm : NodeSeq = {
  getSingleton.toForm(this)
  }
  */
  
  def delete_! : boolean = {
    if (!db_can_delete_?) false else
    runSafe {
      was_deleted_? = getSingleton.delete_!(this)
      was_deleted_?
    }
  }
  
  
  /**
  * Present the model as a form and execute the function on submission of the form
  *
  * @param button - If it's Full, put a submit button on the form with the value of the parameter
  * @param onSuccess - redirect to the URL if the model validates, otherwise display the errors
  *
  * @return the form
  */
  def toForm(button: Can[String], onSuccess: String): NodeSeq =
  toForm(button, (what: A) => {what.validate match {
    case Nil => what.save ; S.redirectTo(onSuccess)
    case xs => S.error(xs)
  }})
  
  /**
  * Append the JSON representation of this model object to the string builder
  * @param the string builder to append the JSON representation of this model to
  *
  * @return the StringBuilder 
  */
  def asJSON(sb: StringBuilder): StringBuilder = {
    getSingleton.asJSON(this, sb)
    sb
  }
  
  /**
  * Create a JSON representation of this model object
  */
  def asJSON: String = asJSON(new StringBuilder).toString
  
  
  /**
  * Present the model as a form and execute the function on submission of the form
  *
  * @param button - If it's Full, put a submit button on the form with the value of the parameter
  * @param f - the function to execute on form submission
  *
  * @return the form
  */
  def toForm(button: Can[String], f: A => Any): NodeSeq =
  getSingleton.toForm(this) ++ (<input type='hidden' name={S.mapFunc((ignore: List[String]) => f(this))} value="n/a" />) ++
  (button.map(b => (<tr><td>&nbsp;</td><td><input type="submit" value={b}/></td></tr>)) openOr scala.xml.Text(""))
  
  def saved_? : Boolean = getSingleton.saved_?(this)
  
  /**
  * Can this model object be deleted?
  */
  def db_can_delete_? : Boolean =  getSingleton.saved_?(this) && !was_deleted_?
  
  def dirty_? : Boolean = getSingleton.dirty_?(this)
  
  override def toString = {
    val ret = new StringBuilder
    
    ret.append(this.getClass.getName)
    
    ret.append("={")
    
    ret.append(getSingleton.appendFieldToStrings(this))
    
    ret.append("}")
    
    ret.toString
  }
  
  def toXml: NodeSeq = {
    getSingleton.toXml(this)
  }
  
  def checkNames {
    runSafe {
      getSingleton match {
        case null => 
        case s => s.checkFieldNames(this)
      }
    }
  }
  
  def comparePrimaryKeys(other: A) = false
  
  /**
  * Find the field by name
  * @param fieldName -- the name of the field to find
  *
  * @return Can[MappedField]
  */ 
  def fieldByName[T](fieldName: String): Can[MappedField[T, A]] = getSingleton.fieldByName[T](fieldName, this)  
  
  type FieldPf = PartialFunction[String, NodeSeq => NodeSeq]
  
  def fieldMapperPf(transform: (BaseOwnedMappedField[A] => NodeSeq)): FieldPf = {
    getSingleton.fieldMapperPf(transform, this)
  }
  
  private var fieldPf_i: FieldPf = Map.empty
  
  def fieldPf = fieldPf_i
  
  def addFieldAfter(pf: FieldPf) {
    fieldPf_i = fieldPf_i orElse pf
    fieldPf_i
  }
  
  def addFieldBefore(pf: FieldPf) {
    fieldPf_i = pf orElse fieldPf_i
    fieldPf_i
  }
  
  /**
  * If there's a field in this record that defines the locale, return it
  */
  def localeField: Can[MappedLocale[A]] = Empty
  
  def timeZoneField: Can[MappedTimeZone[A]] = Empty
  
  def countryField: Can[MappedCountry[A]] = Empty
}

trait LongKeyedMapper[OwnerType <: LongKeyedMapper[OwnerType]] extends KeyedMapper[Long, OwnerType] { self: OwnerType =>
  
}

trait KeyedMapper[KeyType, OwnerType<:KeyedMapper[KeyType, OwnerType]] extends Mapper[OwnerType] { self: OwnerType =>
  def primaryKeyField: MappedField[KeyType, OwnerType] with IndexedField[KeyType];
  def getSingleton: KeyedMetaMapper[KeyType, OwnerType];
  
  override def comparePrimaryKeys(other: OwnerType) = primaryKeyField.is == other.primaryKeyField.is
  
  def reload: OwnerType = getSingleton.find(By(primaryKeyField, primaryKeyField)) openOr this    
  
  def asSafeJs(f: KeyObfuscator): JsExp = getSingleton.asSafeJs(this, f)
  
  override def equals(other: Any): Boolean = {
    other match {
      case null => false
      case km: KeyedMapper[Nothing, Nothing] if this.getClass.isAssignableFrom(km.getClass) ||
      km.getClass.isAssignableFrom(this.getClass) => this.primaryKeyField == km.primaryKeyField
      case k => super.equals(k)
    }
  }
}

