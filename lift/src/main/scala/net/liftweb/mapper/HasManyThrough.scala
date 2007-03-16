package net.liftweb.mapper

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.util.Lazy
import scala.collection.mutable.HashSet

class HasManyThrough[ From<:(KeyedMapper[ThroughType, From] ), To<:Mapper[To], Through<:Mapper[Through], ThroughType<:Any](owner: From,
															   otherSingleton: MetaMapper[To], through: MetaMapper[Through],
															   throughFromField: MappedField[ThroughType, Through], 
															   throughToField: MappedField[ThroughType, Through]) extends LifecycleCallbacks 
{

  private var theSetList: Seq[ThroughType] = Nil
  
  private val others = Lazy[List[To]] {
    DB.use {
    conn =>
    val query = "SELECT DISTINCT "+otherSingleton.dbTableName+".* FROM "+otherSingleton.dbTableName+","+
    through.dbTableName+" WHERE "+
    otherSingleton.dbTableName+"."+otherSingleton.indexedField(otherSingleton.asInstanceOf[To]).get.dbColumnName+" = "+
    through.dbTableName+"."+throughToField.dbColumnName+" AND "+
    through.dbTableName+"."+throughFromField.dbColumnName+" = ?"
    DB.prepareStatement(query, conn) {
      st =>
	owner.getSingleton.indexedField(owner).map {
	  indVal =>
	    st.setObject(1, indVal.getJDBCFriendly, indVal.getTargetSQLType)
	  DB.exec(st) {
	    rs =>
	      otherSingleton.createInstances(rs, None, None)
	  }
	} getOrElse Nil
    }
  }
  }
  
  def apply(): List[To] = others.get
  
  def get: List[To] = this()
  

  def :=(what: Seq[ThroughType]): Seq[ThroughType] = {
    theSetList = what
    theSetList
  }

  override def beforeDelete {
    through.findAll(ByField[Through, ThroughType](throughFromField, owner.primaryKeyField)).foreach {
      toDelete => toDelete.delete_!
    }
  }
  
  override def afterUpdate {
    val current = through.findAll(ByField(throughFromField, owner.primaryKeyField.get))
    
    val newKeys = new HashSet[ThroughType];
    
    theSetList.foreach(i => newKeys += i)
    val toDelete = current.filter(c => !newKeys.contains(throughToField.getActualField(c).get))
    toDelete.foreach{toDel => toDel.delete_!}
    
    val oldKeys = new HashSet[ThroughType];
    current.foreach(i => oldKeys += throughToField.getActualField(i).get)

    theSetList.toList.removeDuplicates.filter(i => !oldKeys.contains(i)).foreach {
      i =>
	val toCreate = through.createInstance
      throughFromField.getActualField(toCreate) := owner.primaryKeyField
      throughToField.getActualField(toCreate) := i
      toCreate.save
    }

    theSetList = Nil
    others.reset
    super.afterUpdate
  }
  
  override def afterCreate {
    theSetList.toList.removeDuplicates.foreach {
      i =>
	val toCreate = through.createInstance
      throughFromField.getActualField(toCreate) := owner.primaryKeyField
      throughToField.getActualField(toCreate) := i
      toCreate.save
    }
    theSetList = Nil
    others.reset
    super.afterCreate
  }
}