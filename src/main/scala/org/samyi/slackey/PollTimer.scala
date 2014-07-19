package org.samyi.slackey

import akka.actor.{ActorLogging, Actor}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author sam
 *
 */

class PollTimer extends Actor with ActorLogging {

    override def preStart() = {
        log.info("Initializing poll timer")
    }

    def receive = {
        case Expiry(poll) => {
            val delayMs = Math.max(0, poll.start + (poll.timeout * 1000) - System.currentTimeMillis())
            context.system.scheduler.scheduleOnce(Duration(delayMs, MILLISECONDS), self, sender ! Expiry(poll))
        }
    }
}
