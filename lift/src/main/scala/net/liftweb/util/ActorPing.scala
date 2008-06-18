package net.liftweb.util

/* 
* Copyright 2007 WorldWide Conferencing, LLC
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

import scala.actors.{Actor, Exit}
import scala.actors.Actor._
import java.util.concurrent._

/** 
* The ActorPing object schedules an actor to be ping-ed with a given message at specific intervals.
* The schedule methods return a ScheduledFuture object which can be cancelled if necessary
*/
object ActorPing {
  
  /** underlying <code>ScheduledExecutor</code> from the java concurrency library */
  private var service = Executors.newSingleThreadScheduledExecutor(TF)
  
  /** 
  * recreates the underlying <code>SingleThreadScheduledExecutor</code>
  */
  def restart: Unit = { service = Executors.newSingleThreadScheduledExecutor(TF) }
  
  /** 
  * shutdown the underlying <code>SingleThreadScheduledExecutor</code>
  */
  def shutdown: Unit = { service.shutdown }
  
  /** 
  * @return a <code>ScheduledFuture</code> sending the <code>msg</code> to the <code>to<code> Actor, every <code>delay</code> milliseconds
  */
  // def schedule(to: Actor, msg: Any, delay: TimeHelpers.TimeSpan): ScheduledFuture[AnyRef] = schedule(to, msg, delay)
  
  /** 
  * @return a <code>ScheduledFuture</code> sending the <code>msg</code> to the <code>to<code> Actor, 
  * every <code>delay</code> using <code>tu<code> as a TimeUnit
  */
  def schedule(to: Actor, msg: Any, delay: TimeSpan): ScheduledFuture[AnyRef] = {
    val r = new java.util.concurrent.Callable[AnyRef] { def call: AnyRef = { to ! msg; null } }
    try {
      service.schedule(r, delay.millis, TimeUnit.MILLISECONDS)
    }
    catch { case e => throw ActorPingException(msg + " could not be scheduled on " + to, e)}
  }
  
  /** 
  * Sends the <code>msg</code> to the <code>to<code> Actor, 
  * after <code>initialDelay</code> and hen subsequently every <code>delay</code> using <code>tu<code> as a TimeUnit
  */
  def scheduleAtFixedRate(to: Actor, msg: Any, initialDelay: TimeSpan, delay: TimeSpan) {
    try {
      val future = service.scheduleAtFixedRate(new java.lang.Runnable { 
        def run = {
          to ! msg; 
        } 
      }, initialDelay.millis, delay.millis, TimeUnit.MILLISECONDS)
      actor {
        self.link(to)
        self.trapExit = true
        to ! Scheduled 
        loop {
          react {
            case UnSchedule | Exit(_, _) => 
            future cancel(true);
            self.unlink(to)
            exit
          }
        }
      }
    }
    catch { case e => throw ActorPingException(msg + " could not be scheduled on " + to, e)}
  }
  
}
/**
* Send by the scheduled actor to sign off from recurrent scheduling
*/
case object UnSchedule
/**
* Send to the actor that we scheduled for recurrent ping
*/
case object Scheduled

/** ActorPing Exception thrown if the ping can't be scheduled */
case class ActorPingException(msg: String, e: Throwable) extends RuntimeException(msg, e)

private object TF extends ThreadFactory {
  val threadFactory = Executors.defaultThreadFactory()
  def newThread(r: Runnable) : Thread = {
    val d: Thread = threadFactory.newThread(r)
    d setName "ActorPing"
    d setDaemon true
    d
  }
}
