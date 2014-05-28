/**
 * @author sam
 *
 */
class Poller {

    case class Poll(id: Int, author: String, question: String, choices: Array[String], anon: Boolean, timeout: Int, var votes: Map[String, String])
    var polls: List[Poll] = List()

    def processPoll(params: Map[String, String]): Option[String] = {
        def get(key: String): String = {
            params.getOrElse(key, "(unknown " + key + ")")
        }

        def getPollSummary(p: Poll): String = {
            (if (p.anon) "Anonymous poll! " else p.author + " asks: ") + p.question + " (ttl=" + p.timeout + ").  Choices: " + p.choices.mkString(", ") + ".  Type \"/vote " + p.id + " <choice>\" to vote!"
        }

        def processNewPoll(params: String, anon: Boolean): Option[String] = {
            val firstQuoteIndex: Int = params.indexOf('\"')
            val lastQuoteIndex: Int = params.lastIndexOf('\"')
            val timeoutParam: String = if (firstQuoteIndex != -1) params.substring(0, firstQuoteIndex).trim else "300"
            val timeout: Int = if (timeoutParam.forall(_.isDigit)) timeoutParam.toInt else 300
            val question: String = if (lastQuoteIndex > firstQuoteIndex) params.substring(firstQuoteIndex + 1, lastQuoteIndex) else ""
            val choices: Array[String] = if (params.length > lastQuoteIndex+1) params.substring(lastQuoteIndex+1).split(",").map(_.trim) else Array()
            if (choices.length > 0) {
                val newPoll = Poll(polls.length+1, get("user_name"), question, choices, anon, timeout, Map())
                polls = polls :+ newPoll
                post(getPollSummary(newPoll))
                None
            } else {
                Some("Invalid poll, please seek help")
            }
        }

        def viewPoll(arg: String): Option[String] = {
            polls.find(p => p.id.toString == arg) match {
                case Some(p: Poll) => Some(getPollSummary(p))
                case None => Some("Poll id \"" + arg + "\" not found!")
            }
        }

        val command: String = Web.decode(get("text")).trim
        if (command.startsWith("new ")) {
            processNewPoll(command.stripPrefix("new ").trim, anon = false)
        } else if (command.startsWith("newanon ")) {
            processNewPoll(command.stripPrefix("newanon ").trim, anon = true)
        } else if (command.startsWith("view ")) {
            viewPoll(command.stripPrefix("view ".trim))
        } else if (command.startsWith("list")) {
            Some("Active polls: <not implemented yet>")
        } else if (command.startsWith("help ")) {
            val helpCommand: String = get("text").stripPrefix("help ").trim
            val directMessage: String = if (helpCommand == "newanon") {
                "Creates a new anonymous poll.  The poll creator and voters will not be identified.\nUsage: /poll newanon <ttl in seconds> \"Poll question\" <comma-separated list of choices>\nExample: /poll newanon 60 \"Whose mom is hottest?\" alice, bob, carol"
            } else if (helpCommand == "new") {
                "Creates a new poll.\nUsage: /poll new [ttl in seconds (default 300)] \"Poll question\" <comma-separated list of choices>\nExample: /poll new 120 \"What should I drink?\" coke, coffee, water, suicide"
            } else if (helpCommand == "view") {
                "Views an existing poll with responses so far.\nUsage: /poll view <poll id>\nExample: /poll view 19"
            } else if (helpCommand == "list") {
                "Lists all active polls.\nUsage: /poll list"
            } else {
                "Supported commands: new, newanon, view, list.  Type /poll help <command> for info."
            }
            Some(directMessage)
        } else {
            val directMessage: String = "Supported commands: new, newanon, view, list.  Type /poll help <command> for info."
            Some(directMessage)
        }
    }

    def processVote(params: Map[String, String]): Option[String] = {
        def get(key: String): String = {
            params.getOrElse(key, "(unknown " + key + ")")
        }

        val voteParams:Array[String] = Web.decode(get("text")).trim.split(" ", 1)
        if (voteParams.length == 2 && voteParams(0).forall(_.isDigit)) {
            polls.find(_.id.toString == voteParams(0)) match {
                case Some(p: Poll) => if (p.choices.contains(voteParams(1))) {
                    p.votes = p.votes + ((get("user_name"), voteParams(1)))
                    Some("Vote cast!")
                } else {
                    Some(voteParams(1) + " is not a valid choice!")
                }
                case None => Some("Poll id " + voteParams(0) + " not found!")
            }
        } else {
            Some("Invalid vote.  Please seek help.")
        }
    }

    def post(text: String) = {
        val postParams: List[(String, String)] = List(("username", "pollbot"), ("icon_emoji", ":bar_chart:"), ("text", text))
        Web.sendToChannel(postParams)
    }
}
