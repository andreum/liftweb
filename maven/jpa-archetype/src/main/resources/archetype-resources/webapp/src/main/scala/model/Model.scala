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

import java.sql.SQLException
import org.hibernate.HibernateException

import net.liftweb.util.Log

object Model extends ScalaEntityManager("jpaweb") {
  lazy val factory = Persistence.createEntityManagerFactory(persistanceName)

  override def openEM () = {
    val em = factory.createEntityManager()
    em.getTransaction().begin()
    em
  }

  override def closeEM (em : EntityManager) = {
    if (em.getTransaction().getRollbackOnly()) {
      em.getTransaction().rollback()
    } else {
      em.getTransaction().commit()
    }

    em.close()
  }

  /**
   This method allows me to clean up my code a bit and only handle JPA-related exceptions.
   An example usage would be:

   def addFood(newFood : Food) =
     wrapEM({
       Model.persist(newFood)
       S.redirectTo("/food/list")
     }, {
       case cve : ConstraintViolationException => S.error("That food already exists!")
       case _ => S.error("Internal error adding food")
     })

   Note that if I used normal try/catch then the wildcard match would trap the RedirectException
   thrown by S.redirectTo.
  */
  def wrapEM(f : => Unit) : Unit = wrapEM(f, { case _ => /* nop */ })
  def wrapEM[A](f : => A, handler : PartialFunction[Throwable, A]) : A = {
    try {
      val tx = getEM.getTransaction()

      if (! tx.isActive() ) { tx.begin() }
      try {
	val ret : A = f
	ret
      } catch {
	case he : HibernateException => {
	  Log.error("Hibernate error", he)
	  handler(he)
	}
	case pe : PersistenceException => {
	  Log.error("EM Error", pe)
	  handler(pe)
	}
	case sqle : java.sql.SQLException => {
	  Log.error("SQL Exception", sqle)
	  handler(sqle)
	}
      } finally {
	// make sure that we commit even with a redirectexception
	if (tx.isActive() && ! tx.getRollbackOnly()) {
	  tx.commit()
	} else if (tx.getRollbackOnly()) {
	  tx.rollback()
	}
      }
    } catch {
      // Special case. Usually we want to know why it failed to commit, not just that it failed
      case re : RollbackException => {
	val (cause,message) = if (re.getCause() == null) {
	  (re,"No cause")
	} else {
	  (re.getCause(), re.getCause().getMessage())
	}
	Log.error("EM Commit error", re)
	handler(cause)
      }
    }
  }
}

