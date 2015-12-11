package org.samyi.slackey.bots.casbot

import akka.actor.{ActorRef, Actor, Props}
import akka.pattern.ask
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

case class Card(value: Int, suit: Int) {
  def getValueString: String = value match {
    case 1 => "*A*"
    case 11 => "*J*"
    case 12 => "*Q*"
    case 13 => "*K*"
    case v => s"*${v.toString}*"
  }

  def getSuitString: String = suit match {
    case 1 => ":hearts:"
    case 2 => ":clubs:"
    case 3 => ":diamonds:"
    case 4 => ":spades:"
    case _ => ":skull:"
  }
}

object Deck {
  def props = Props(classOf[Deck])
  case object GetCard
  case class NextCard(card: Card)
}

class Deck extends Actor {
  import Deck._
  var usedCards: Set[Card] = Set.empty
  def receive = {
    case GetCard => ???
  }
}

// 1P for now
object Blackjack {
  def props(player: ActorRef) = Props(classOf[Blackjack], player)
  case class Ante(amt: Long)
  case object Hit
  case object Stand
  case object Fold
  trait Result
  case object Win extends Result
  case object Lose extends Result
  case class Status(dealerCards: List[Card], playerCards: List[Card], result: Option[Result])
}

class Blackjack(player: ActorRef) extends Actor {
  import Blackjack._
  var dealerCards: List[Card] = List.empty
  var playerCards: List[Card] = List.empty

  val deck = context.system.actorOf(Deck.props)

  def deal() = {

  }

  def getCardValue(card: Card, high: Boolean): Int = card.value match {
    case 1 => if (high) 11 else 1
    case v if v < 11 => v
    case _ => 10
  }

  def getTotal(cards: List[Card]) = {
    cards.foldLeft(0)((sum, card) => sum + getCardValue(card, high = true))
  }

  def getResult = {
    ???
  }

  def getStatus = Status(dealerCards, playerCards, getResult)

  def receive = {
    case Ante(amt) =>
      // check UpdateBalance here?
      context become dealing
  }

  def dealing: Receive = {
    case Deck.NextCard(card) if dealerCards.length == 2 && playerCards.length == 2 =>
      // check
      context become waitingPlayer
    case Deck.NextCard(card) =>

  }

  def waitingPlayer: Receive = {
    case Hit =>
      deck ! Deck.GetCard
      context.become(waitingPlayerCard)
    case Stand =>
    case Fold =>
  }

  def waitingPlayerCard: Receive = {
    case Deck.NextCard(card) =>
      playerCards = playerCards :+ card
      context.unbecome()
  }

  def dealersTurn: Receive = {
    case Deck.NextCard(card) =>
  }
}