package org.samyi.slackey

import akka.actor.{ActorSystem, Props}

/**
 * @author sam
 *
 */
case class Poll(id: Int, question: String, choices: List[String], anon: Boolean, ptype: String,
                start: Long, timeout: Int, expired: Boolean, author: String, channel: String)

case class Vote(pollId: Int, voter: String, choice: String, time: Long)

object Poller {

    val db = new PollDBConnection(Web.mongoURI, Web.mongoDbName)

    val system = ActorSystem("PollSystem")
    val pollTimer = system.actorOf(Props[PollTimer], name = "pollactor")

    // Poll functions

    def printChoices(p: Poll): String = {
        (for (i <- p.choices.indices) yield s"(${('a'+i).toChar}) ${p.choices(i)}").mkString(", ")
    }

    def getPollSummary(p: Poll): String = {
        val auth = if (p.anon) "Anonymous poll:" else s"${p.author} asks:"
        "(Poll " + p.id + ") " + auth + " \"" + p.question + "\""
    }

    def getPollDetails(p: Poll): String = {
        val sChoices: String = s" Choices: ${printChoices(p)}. "
        val sVote: String = "\"/vote " + p.id + " <choice_letter>\" to vote!"
        val sTimeout: String = s" (ttl=${if (p.timeout == 0) "infinite" else s"${p.timeout}s"})"
        getPollSummary(p) + sChoices + sVote + sTimeout
    }

    def getPollTally(p: Poll): String = {
        val res: Map[String, List[Vote]] = db.getResults(p)

        def votesFor(choice: String) = {
            val votes = res.getOrElse(choice, List())
            votes.size.toString +
                (if (p.anon || votes.size == 0) "" else s" (${votes.map(_.voter).mkString(", ")})")
        }

        val lines = for (i <- p.choices.indices; c = p.choices(i))
        yield s"\n(${('a' + i).toChar}) $c: ${votesFor(c)}"

        lines.mkString("\n")
    }

    def getPollWinners(p: Poll): String = {
        val res: Map[String, List[Vote]] = db.getResults(p)
        val winCount = res.map(c => c._2.length).max
        def style(c: (String, List[Vote])) = if (c._2.length == winCount) "*" else ""

        res.map(c => s"${style(c)}${c._1} (${c._2.length})${style(c)}").mkString(", ")
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
            registerExpiry(newPollWithId)
            post(getPollDetails(newPollWithId))
            None
        } else {
            Some("Invalid poll, please seek help")
        }
    }

    def viewPoll(arg: String): String = {
        if (arg.forall(_.isDigit)) {
            db.getPoll(arg.toInt) match {
                case Some(poll) => getPollDetails(poll) + "\n" + getPollTally(poll)
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
            "Lists all active polls. Specify \"all\" to view all polls.  \nUsage: /poll list [all]"
        } else {
            "Supported commands: new, newanon, view, list.  Type /poll help <command> for info."
        }
    }

    def listPolls(arg: String): String = {
        val all = arg.startsWith("all")
        val polls = if (all)
            db.getPolls.map(p => getPollSummary(p)).mkString("\n")
        else
            db.getActivePolls.map(p => getPollSummary(p)).mkString("\n")

        val intro = if (all) "All polls: " else "Active polls: "

        intro + (if (polls.isEmpty) "(none)" else s"\n$polls")
    }

    def expirePoll(p: Poll) = {
        db.expirePoll(p.id)
        post(s"${getPollSummary(p)} has expired!  Results: ${getPollWinners(p)}")
    }

    def registerExpiry(p: Poll) = if (p.timeout != 0) pollTimer ! Expiry(p)

    def registerExpiries() = db.getActivePolls foreach registerExpiry

    def processPoll(params: Map[String, String]): Option[String] = {
        def get(key: String): String = {
            params.getOrElse(key, s"(unknown $key)")
        }

        val command: String = Web.decode(get("text")).trim
        if (command.startsWith("new ")) {
            processNewPoll(command.stripPrefix("new ").trim, anon = false, get("user_name"), get("channel"))
        } else if (command.startsWith("newanon ")) {
            processNewPoll(command.stripPrefix("newanon ").trim, anon = true, get("user_name"), get("channel"))
        } else if (command.startsWith("view ")) {
            Some(viewPoll(command.stripPrefix("view ").trim))
        } else if (command.startsWith("list")) {
            Some(listPolls(command.stripPrefix("list ").trim))
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
            params.getOrElse(key, s"(unknown $key)")
        }

        val voteParams: Array[String] = Web.decode(get("text")).trim.split(" ", 2)
        if (voteParams.length == 2 && voteParams(0).forall(_.isDigit)) {
            val pollId = voteParams(0).toInt
            db.getPoll(pollId) match {
                case Some(p: Poll) => {
                    val i: Int = voteParams(1).trim.charAt(0).toLower - 'a'
                    if (p.expired) {
                        Some(s"Sorry, poll ${p.id} expired!")
                    } else if (i >= 0 && p.choices.length > i) {
                        db.vote(Vote(pollId, get("user_name"), p.choices(i), System.currentTimeMillis()))
                        Some("Vote cast for \"" + p.choices(i) + "\"!")
                    } else {
                        Some(s"${voteParams(1)} is not a valid choice!")
                    }
                }
                case None => Some(s"Poll id ${voteParams(0)} not found!")
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
}
