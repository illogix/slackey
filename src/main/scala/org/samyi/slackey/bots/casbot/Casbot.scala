package org.samyi.slackey.bots.casbot

import akka.actor.{ActorRef, Actor, Props}
import org.samyi.slackey.Req

/**
  * Created by illo on 12/9/15.
  */
trait Response
case object OK extends Response
case class Error(msg: String) extends Response

object Casbot {
  def props(stuff: String) = Props(classOf[Casbot], stuff)
  case class AddPlayer(name: String)
}

class Casbot(stuff: String) extends Actor {
  import Casbot._

  var players: Map[String, ActorRef] = Map.empty

   def receive = {
     case Req(params) =>

     case AddPlayer(name) =>
       players get name match {
         case Some(player) => sender() ! Error(s"Player $player already exists!")
         case None => players = players + (name -> context.system.actorOf(Player.props(name)))
       }
   }

}


object Player {
  def props(name: String) = Props(classOf[Player], name, 0L)
  case class UpdateBalance(diff: Long)
}

class Player(name: String, var balance: Long) extends Actor {
  import Player._

  def receive = {
    case UpdateBalance(diff) =>
      if (balance + diff < 0) {
        sender() ! Error(s"Insufficient balance for $name")
      } else {
        balance = balance + diff
        sender() ! OK
      }
  }
}


object Blackjack {
  def props = Props(classOf[Blackjack])
}

class Blackjack {

}