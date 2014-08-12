package org.samyi.slackey

import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.{Http, Response}
import com.twitter.finagle.Service
import com.twitter.util.Future

import java.net.{InetSocketAddress, URLDecoder}
import java.nio.charset.Charset

import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse}

import scalaj.http.{Http => JHttp}
import util.Properties

object Web {
    // These should be set in Heroku as config vars
    val outWebHookToken: String = System.getenv("OUT_WEBHOOK_TOKEN")
    val slashRollToken: String = System.getenv("SLASH_ROLL_TOKEN")
    val slashPollToken: String = System.getenv("SLASH_POLL_TOKEN")
    val slashVoteToken: String = System.getenv("SLASH_VOTE_TOKEN")
    val inWebHookURL: String = System.getenv("IN_WEBHOOK_URL")
    val mongoURI: String = System.getenv("MONGO_MACK_URI")
    val mongoDbName: String = System.getenv("MONGO_MACK_DB")


    def decode(urlEncoded: String): String = {
        URLDecoder.decode(urlEncoded, "UTF-8")
    }

    def makeJson(kvs: List[(String, String)]): String = {
        def makeInside(kv: (String, String)): String = {
            "\"" + kv._1 + "\": \"" + kv._2.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
        }
        val inside = kvs map ((kv: (String, String)) => makeInside(kv)) mkString ", "
        "{" + inside + "}"
    }

    def sendToChannel(postParams: List[(String, String)]) = {
        JHttp.post(Web.inWebHookURL).params("payload" -> Web.makeJson(postParams)).asString
    }


    def main(args: Array[String]) {
        val port = Properties.envOrElse("PORT", "8080").toInt
        println("Starting on port:" + port)
        ServerBuilder()
            .codec(Http())
            .name("slackey-server")
            .bindTo(new InetSocketAddress(port))
            .build(new Slackey)
        println("Started.")
        init()
    }


    def init() = {
        Poller.scheduleExpiries()
    }
}

class Slackey extends Service[HttpRequest, HttpResponse] {

    val dice: Dice = new Dice

    def apply(req: HttpRequest): Future[HttpResponse] = {
        val postParams: List[String] = req.getContent.toString(Charset.forName("UTF-8")).split("&").toList
        val params = postParams.map(pp => {
            pp.split("=") match {
                case Array(x, y) => (x, y)
                case Array(x) => (x, "")
                case _ => ("", "")
            }
        }).toMap
        process(params)
    }

    def process(params: Map[String, String]): Future[HttpResponse] = {
        def get(key: String): String = {
            params.getOrElse(key, "(unknown " + key + ")")
        }

        get("token") match {
            case Web.outWebHookToken => Web.decode(get("text")).split("\\s+")(0) match {
                case "!vote" => respond(Poller.processVote(params, slash = false), json = false)
                case _ => respond(None)
            }
            case Web.slashRollToken => respond(dice.process(params), json = false)
            case Web.slashPollToken => respond(Poller.processPoll(params), json = false)
            case Web.slashVoteToken => respond(Poller.processVote(params), json = false)
            case _ => respond(None)
        }
    }

    def respond(resp: Option[String], json: Boolean = true): Future[HttpResponse] = {
        val response = Response()
        response.setStatusCode(200)
        resp match {
            case Some(respText) => response.setContentString(if (json) Web.makeJson(List(("text", respText))) else respText)
            case None =>
        }
        Future(response)
    }
}
