/*
 * Copyright 2008 WorldWide Conferencing, LLC
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
package net.liftweb.jpademo.model

import javax.persistence._
import scala.collection.jcl.{BufferWrapper,SetWrapper}
import net.liftweb.http.RequestVar
import net.liftweb.util.{Box,Full,Empty}
import java.util.{Date,Calendar}

object JPA {
  implicit def setToWrapper[A](set : java.util.Set[A]) = new SetWrapper[A]{override def underlying = set}
  implicit def listToWrapper[A](list : java.util.List[A]) = new BufferWrapper[A]{override def underlying = list}

  def findToBox[A](f: => A): Box[A] =
    try {
      f match {
        case found: A => Full(found)
        case null => Empty
      }
    } catch {
      case e: NoResultException => Empty
    }

}

abstract class ScalaEntityManager(val persistanceName: String) {
  // The concrete impl should provide these methods
  protected def openEM () : EntityManager
  protected def closeEM (em : EntityManager)

  private object emVar extends RequestVar(openEM()) {
    override def cleanupFunc : Box[() => Unit]= Full(() => closeEM(this.is))
  }

  // dont encourage use of the entity manager directly
  def getEM  = em
  private def em = emVar.is

  // value added methods
  def findAll[A](queryName : String, params : Pair[String,Any]*) = createAndParamify[A](queryName, params).findAll
  def createNamedQuery[A](queryName : String, params : Pair[String,Any]*) : ScalaQuery[A] = createAndParamify[A](queryName,params)

  // Worker for the previous two methods
  private def createAndParamify[A](queryName : String, params : Seq[Pair[String,Any]]) : ScalaQuery[A] = {
    val q = createNamedQuery[A](queryName)
    params.foreach(param => q.setParameter(param._1, param._2))
    q
  }

  // methods defined on Entity Manager
  def persist(entity: AnyRef) = em.persist(entity)
  def merge[T](entity: T): T = em.merge(entity)
  def remove(entity: AnyRef) = em.remove(entity);
  def find[A](clazz: Class[A], id: Any) = em.find[A](clazz, id).asInstanceOf[A]
  def flush() = em.flush()
  def setFlushMode(flushModeType: FlushModeType) = em.setFlushMode(flushModeType)
  def refresh(entity: AnyRef) = em.refresh(entity)
  def getFlushMode() = em.getFlushMode()
  def createQuery[A](queryString: String) = new ScalaQuery[A](em.createQuery(queryString))
  def createNamedQuery[A](queryName: String) = new ScalaQuery[A](em.createNamedQuery(queryName))
  def createNativeQuery[A](sqlString: String) = new ScalaQuery[A](em.createNativeQuery(sqlString))
  def createNativeQuery[A](sqlString: String, clazz: Class[A]) = new ScalaQuery[A](em.createNativeQuery(sqlString, clazz))
  def createNativeQuery[A](sqlString: String, resultSetMapping: String) = new ScalaQuery[A](em.createNativeQuery(sqlString, resultSetMapping))
  def close() = em.close()
  def isOpen() = em.isOpen()
  def getTransaction() = em.getTransaction()
  def joinTransaction() = em.joinTransaction()
  def clear() = em.clear()
  def getDelegate() = em.getDelegate()
  def getReference[A](clazz: Class[A], primaryKey: Any) = em.getReference[A](clazz, primaryKey)
  def lock(entity: AnyRef, lockMode: LockModeType) = em.lock(entity, lockMode)
  def contains(entity: AnyRef) = em.contains(entity)
}

class ScalaQuery[A](val query: Query) {

  // value added methods
  def findAll = getResultList()
  def findOne = JPA.findToBox[A](query.getSingleResult.asInstanceOf[A])
  def setParams(params : Pair[String,Any]*) = {params.foreach(param => query.setParameter(param._1, param._2)); this}

  // methods defined on Query
  def getResultList() = JPA.listToWrapper[A](query.getResultList.asInstanceOf[java.util.List[A]])
  def getSingleResult() = query.getSingleResult.asInstanceOf[A]
  def executeUpdate() = query.executeUpdate()
  def setMaxResults(maxResult: Int) = {query.setMaxResults(maxResult);this}
  def setFirstResult(startPosition: Int) = {query.setFirstResult(startPosition); this}
  def setHint(hintName: String, value: Any) = {query.setHint(hintName, value); this}
  def setParameter(name: String, value: Any) = {query.setParameter(name, value); this}
  def setParameter(position: Int, value: Any) = {query.setParameter(position, value); this}
  def setParameter(name: String, value: Date, temporalType: TemporalType) = {query.setParameter(name, value, temporalType); this}
  def setParameter(position: Int, value: Date, temporalType: TemporalType) = {query.setParameter(position, value, temporalType); this}
  def setParameter(name: String, value: Calendar, temporalType: TemporalType) = {query.setParameter(name, value, temporalType); this}
  def setParameter(position: Int, value: Calendar, temporalType: TemporalType) = {query.setParameter(position, value, temporalType); this}
  def setFlushMode(flushMode: FlushModeType) = {query.setFlushMode(flushMode); this}

}



