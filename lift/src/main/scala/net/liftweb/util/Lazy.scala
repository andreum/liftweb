package net.liftweb.util

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

/**
 * A class that does lazy evaluation
 *
 * @param f -- a function that evaluates to the default value of the instance
 */
class FatLazy[T](f: => T) {
  private var value: Can[T] = Empty
  
  /**
   * Get the value of the instance.  If it's not yet been set, call f to calculate it
   *
   * @return the value of the instance
   */
  def get: T = synchronized {
    value match {
      case Full(v) => v
      case _ => value = Full(f)
      value.open_!
    }
  }
  
  def defined_? = synchronized {
    value != None
  }
  
 
  /**
   * Set the instance to a new value and return that value
   *
   * @param v - the new value of the instance
   *
   * @return v
   */
  def set(v: T): T = synchronized {
    value = Full(v)
    v
  }
  
  def setFrom(other: FatLazy[T]): Unit = synchronized {
    value = other.value
  }
  
  /**
   * and the lazy() = foo style of assignment
   */
  def update(v: T): Unit = set(v)
    
  def reset = synchronized {value = Empty}
  
  def calculated_? = synchronized {value.isDefined}
  
  // implicit def fromLazy[T](in: Lazy[T]): T = in.get
}

object FatLazy {
  // implicit def fromLazy[T](in: Lazy[T]): T = in.get
  def apply[T](f: => T) = new FatLazy(f)
}

object ThreadLazy {
  def apply[T](f: => T) = new ThreadLazy(f)
  
  implicit def what[T](in: ThreadLazy[T]): T = in.get
}

/**
  * Sometimes, you want to do pattern matching against a lazy value.  Why?
  * Because, there may be parts of the pattern that must be evaluated first
  * and if they evaluate successfully, you then want to test another part of
  * the pattern.  Thus, the LZ pattern match.
  */
object LZ {
  def apply[T](f: => T): LZ[T] = new LZ(f)
  def unapply[T](in: LZ[T]): Option[T] = Some(in.get)
  
 // implicit def lazyToT[T](in: LazyMatcher[T]): T = in.get
}

/**
  * LZ encapsulates a lazy value.
  *
  * @param f - a value to be evaluated lazily
  */
class LZ[T](f: => T) {
  lazy val get = f
  override def toString = "LZ("+get+")"
}

class ThreadLazy[TheType](theFunc: => TheType) extends LoanWrapper {
  private val calced = new ThreadGlobal[Boolean]
  private val value = new ThreadGlobal[TheType]
  
  def apply[T](f: => T): T = {
    val old = value.value
    calced.set(false)
    try {
      f
    } finally {
      calced.set(false)
      value.set(old)
    }
  }
  
  def reset(): Unit = calced.set(false)
  
  def get: TheType = {
    if (calced.value) value.value
    else {
      value.set(theFunc)
      calced.set(true)
      value.value
    }
  }
  
}

