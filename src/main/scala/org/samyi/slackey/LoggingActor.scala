package org.samyi.slackey

import akka.actor.{Actor, ActorLogging}

/**
 * @author sam
 *
 */
case class Log(msg: String)

class LoggingActor extends Actor with ActorLogging {

    override def preStart() = {
        log.info("Initializing logging actor")
    }

    def receive = {
        case Log(msg) => log.info(msg)
    }

}
