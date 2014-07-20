package org.samyi.slackey

import akka.actor.{ActorLogging, Actor}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author sam
 *
 */

case class Expiry(poll: Poll)

class PollTimer extends Actor with ActorLogging {

    override def preStart() = {
        log.info("Initializing poll timer")
    }

    def receive = {
        case Expiry(poll) => {
            val delayMs = Math.max(0, poll.start + (poll.timeout * 1000) - System.currentTimeMillis())
            log.info(s"Poll ${poll.id}: delayMs=$delayMs")
            context.system.scheduler.scheduleOnce(Duration(delayMs, MILLISECONDS), self, Poller.expirePoll(poll))
        }
    }
}
