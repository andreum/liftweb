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
package net.liftweb.util

import Helpers._
import org.specs._
import org.specs.util.WaitFor
import org.specs.runner._
import scala.actors.Actor
import java.util.concurrent._

class ActorPingSpecTest extends JUnit3(ActorPingSpec)
object ActorPingSpecRunner extends ConsoleRunner(ActorPingSpec)
object ActorPingSpec extends Specification with PingedService with WaitFor {
  "The ActorPing object" should { doBefore { ActorPing.restart }
    "provide a schedule method to ping an actor regularly" in {
      service.start
      ActorPing.schedule(service, Alive, TimeSpan(10))
      waitFor(100.ms)
      service.pinged must beTrue
    }
  }
}
trait PingedService {
  case object Alive
  val service = new Service

  class Service extends Actor {
    var pinged = false
    def act() {
      while (true) {
        receive {
          case Alive => {pinged = true; exit()}
        }
      }
    }
  }
}
