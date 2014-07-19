package org.samyi.slackey

import akka.actor.{Actor, ActorSystem, Props}

/**
 * @author sam
 *
 */
case class Poll(id: Int, question: String, choices: List[String], anon: Boolean, ptype: String,
                start: Long, timeout: Int, expired: Boolean, author: String, channel: String)

case class Vote(pollId: Int, voter: String, choice: String, time: Long)

case class Expiry(poll: Poll)

object Poller {
    val poller = new Poller
    poller.registerPolls()
}

class Poller extends Actor {

    val db = new PollDBConnection(Web.mongoURI, Web.mongoDbName)

    val system = ActorSystem("PollSystem")
    val pollTimer = system.actorOf(Props[PollTimer], name = "pollactor")

    // Poll functions

    def printChoices(p: Poll): String = {
        (for (i <- p.choices.indices) yield "(" + ('a' + i).toChar + ") " + p.choices(i)).mkString(", ")
    }

    def getPollSummary(p: Poll): String = {
        "Poll " + p.id + ":" + (if (p.anon) "Anonymous poll: " else p.author + " asks: ") + "\"" + p.question + "\""
    }

    def getPollDetails(p: Poll): String = {
        val sChoices: String = " Choices: " + printChoices(p) + ". "
        val sVote: String = "Type \"/vote " + p.id + " <choice_letter>\" to vote!"
        val sTimeout: String = " (ttl=" + (if (p.timeout == 0) "infinite" else p.timeout + "s") + ")"
        getPollSummary(p) + sChoices + sVote + sTimeout
    }

    def getPollResults(p: Poll): String = {
        val res: Map[String, List[Vote]] = db.getResults(p.id)

        def votesFor(choice: String) = {
            val votes = res.getOrElse(choice, List())
            votes.size.toString +
                (if (p.anon || votes.size == 0) "" else " (" + votes.map(_.voter).mkString(", ") + ")")
        }

        val lines = for (i <- p.choices.indices; c = p.choices(i))
        yield "\n(" + ('a' + i).toChar + ") " + c + ": " + votesFor(c)

        lines.mkString("\n")
    }

    def processNewPoll(params: String, anon: Boolean, author: String, channel: String): Option[String] = {
        val firstQuoteIndex: Int = params.indexOf('\"')
        val lastQuoteIndex: Int = params.lastIndexOf('\"')
        val timeoutParam: String = if (firstQuoteIndex > 0) params.substring(0, firstQuoteIndex).trim else "0"
        val timeout: Int = if (timeoutParam.forall(_.isDigit) && timeoutParam.length <= 8) timeoutParam.toInt else 0
        val question: String = if (lastQuoteIndex > firstQuoteIndex) params.substring(firstQuoteIndex + 1, lastQuoteIndex) else ""
        val choices: List[String] = if (params.length > lastQuoteIndex + 1) params.substring(lastQuoteIndex + 1).split(",").map(_.trim).toList else List()
        if (lastQuoteIndex > firstQuoteIndex && choices.length > 0) {
            val newPoll = Poll(0, question, choices, anon, "type", System.currentTimeMillis(), timeout, expired = false,
                author, channel)
            val newPollWithId = db.addPoll(newPoll)
            post(getPollDetails(newPollWithId))
            None
        } else {
            Some("Invalid poll, please seek help")
        }
    }

    def viewPoll(arg: String): String = {
        if (arg.forall(_.isDigit)) {
            db.getPoll(arg.toInt) match {
                case Some(poll) => getPollDetails(poll) + "\n" + getPollResults(poll)
                case None => "Poll id " + arg + " not found!"
            }
        } else {
            "Invalid poll id " + arg
        }
    }

    def getHelp(command: String): String = {
        if (command == "newanon") {
            "Creates a new anonymous poll.  The poll creator and voters will not be identified.\nUsage: /poll newanon <ttl in seconds> \"Poll question\" <comma-separated list of choices>\nExample: /poll newanon 5 \"Whose mom is hottest?\" alice, bob, carol"
        } else if (command == "new") {
            "Creates a new poll.\nUsage: /poll new [ttl in seconds (0=infinite)] \"Poll question\" <comma-separated list of choices>\nExample: /poll new 10 \"What should I drink?\" coke, coffee, water, suicide"
        } else if (command == "view") {
            "Views an existing poll with responses so far.\nUsage: /poll view <poll id>\nExample: /poll view 19"
        } else if (command == "list") {
            "Lists all active polls.\nUsage: /poll list"
        } else {
            "Supported commands: new, newanon, view, list.  Type /poll help <command> for info."
        }
    }

    def activePolls: String = {
        db.getActivePolls.map(p => getPollSummary(p)).mkString("\n")
    }

    def allPolls: String = {
        db.getPolls.map(p => getPollSummary(p)).mkString("\n")
    }

    def expirePoll(p: Poll) = {
        db.expirePoll(p.id)
        post(getPollSummary(p) + " has expired!  Results:\n" + getPollResults(p))
    }

    def registerExpiry(p: Poll) = pollTimer ! Expiry(p)

    def registerPolls() = db.getActivePolls foreach registerExpiry

    def processPoll(params: Map[String, String]): Option[String] = {
        def get(key: String): String = {
            params.getOrElse(key, "(unknown " + key + ")")
        }

        val command: String = Web.decode(get("text")).trim
        if (command.startsWith("new ")) {
            processNewPoll(command.stripPrefix("new ").trim, anon = false, get("user_name"), get("channel"))
        } else if (command.startsWith("newanon ")) {
            processNewPoll(command.stripPrefix("newanon ").trim, anon = true, get("user_name"), get("channel"))
        } else if (command.startsWith("view ")) {
            Some(viewPoll(command.stripPrefix("view ").trim))
        } else if (command.startsWith("list")) {
            Some(activePolls)
        } else if (command.startsWith("history")) {
            Some(allPolls)
        } else if (command.startsWith("help ")) {
            val helpCommand: String = command.stripPrefix("help ").trim
            Some(getHelp(helpCommand))
        } else {
            val directMessage: String = "Supported commands: new, newanon, view, list.  Type /poll help <command> for info."
            Some(directMessage)
        }
    }


    // Vote functions
    def processVote(params: Map[String, String]): Option[String] = {
        def get(key: String): String = {
            params.getOrElse(key, "(unknown " + key + ")")
        }

        val voteParams: Array[String] = Web.decode(get("text")).trim.split(" ", 2)
        if (voteParams.length == 2 && voteParams(0).forall(_.isDigit)) {
            val pollId = voteParams(0).toInt
            db.getPoll(pollId) match {
                case Some(p: Poll) => {
                    val i: Int = voteParams(1).trim.charAt(0).toLower - 'a'
                    if (i >= 0 && p.choices.length > i) {
                        db.vote(Vote(pollId, get("user_name"), p.choices(i), System.currentTimeMillis()))
                        Some("Vote cast for \"" + p.choices(i) + "\"!")
                    } else {
                        Some(voteParams(1) + " is not a valid choice!")
                    }
                }
                case None => Some("Poll id " + voteParams(0) + " not found!")
            }
        } else {
            Some("Invalid vote.  Correct format is: /vote <poll_id> <choice_letter>")
        }
    }

    // Web
    private def post(text: String) = {
        val postParams: List[(String, String)] = List(("username", "pollbot"), ("icon_emoji", ":bar_chart:"), ("text", text))
        Web.sendToChannel(postParams)
    }

    // Actor
    def receive = {
        case Expiry(poll) => expirePoll(poll)
    }

}
