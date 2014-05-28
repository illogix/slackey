/**
 * @author sam
 *
 */
class Poller {


    def processPoll(params: Map[String, String]): Option[String] = {
        def get(key: String): String = {
            params.getOrElse(key, "(unknown " + key + ")")
        }

        def startsWithCommand(s: String, cmd: String): Boolean = {
            s.startsWith(cmd) || s.startsWith(cmd, 1)
        }

        def newPoll(params: String, anon: Boolean): Option[String] = {
            val firstQuoteIndex = params.indexOf('\"')
            val lastQuoteIndex = params.lastIndexOf('\"')
            val timeoutParam: String = if (firstQuoteIndex != -1) params.substring(0, firstQuoteIndex).trim else "300"
            val timeout: Int = if (timeoutParam.forall(_.isDigit)) timeoutParam.toInt else 300
            val question: String = if (lastQuoteIndex > firstQuoteIndex) params.substring(firstQuoteIndex + 1, lastQuoteIndex) else ""
            val pollid: Int = 123
            val postMessage: String = get("user_name") + " asks: " + question + "\nType \"/poll view " + pollid + "\" to participate!"
            post(postMessage)
            None
        }

        val command: String = Web.decode(get("text")).trim
        if (command.startsWith("!new ")) {
            newPoll(command.stripPrefix("!new ").trim, anon = false)
        } else if (command.startsWith("!newanon ")) {
            newPoll(command.stripPrefix("!newanon ").trim, anon = true)
        } else if (command.startsWith("!view ")) {
            Some("Poll details: <not implemented yet>")
        } else if (command.startsWith("!list")) {
            Some("Active polls: <not implemented yet>")
        } else if (command.startsWith("!help ")) {
            val helpCommand: String = get("text").stripPrefix("%21help+")
            val directMessage: String = if (startsWithCommand(helpCommand, "newanon")) {
                "Creates a new anonymous poll.  The poll creator and voters will not be identified.\nUsage: /poll !newanon <ttl in seconds> \"Poll question\" <comma-separated list of choices>\nExample: /poll !newanon 60 \"Whose mom is hottest?\" alice, bob, carol"
            } else if (startsWithCommand(helpCommand, "new")) {
                "Creates a new poll.\nUsage: /poll !new [ttl in seconds (default 300)] \"Poll question\" <comma-separated list of choices>\nExample: /poll !new 120 \"What should I drink?\" coke, coffee, water, suicide"
            } else if (startsWithCommand(helpCommand, "view")) {
                "Views an existing poll with responses so far.\nUsage: /poll !view <poll id>\nExample: /poll !view 19"
            } else if (startsWithCommand(helpCommand, "list")) {
                "Lists all active polls.\nUsage: /poll !list"
            } else {
                "Supported commands: !new, !newanon, !view, !list.  Type /poll !help <command> for info."
            }
            Some(directMessage)
        } else {
            val directMessage: String = "Supported commands: !new, !newanon, !view, !list.  Type /poll !help <command> for info."
            Some(directMessage)
        }
    }

    def processVote(params: Map[String, String]): Option[String] = {
        def get(key: String): String = {
            params.getOrElse(key, "(unknown " + key + ")")
        }
        Some("You voted, but i'm not counting it, so there :stuck_out_tongue:")
    }

    def post(text: String) = {
        val postParams: List[(String, String)] = List(("username", "pollbot"), ("icon_emoji", ":bar_chart:"), ("text", text))
        Web.sendToChannel(postParams)
    }
}
