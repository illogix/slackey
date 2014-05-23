import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.{Http, Response}
import com.twitter.finagle.Service
import com.twitter.util.Future

import java.net.{InetSocketAddress, URLDecoder}
import java.nio.charset.Charset

import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse}

import util.Properties

object Web {
    // THESE SHOULD BE SET SOMEWHERE AS CONFIG VARS
    val outWebHookToken: String = System.getenv("OUT_WEBHOOK_TOKEN")
    val slashRollToken: String = System.getenv("SLASH_ROLL_TOKEN")
    val inWebHookURL: String = System.getenv("IN_WEBHOOK_URL")

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

    def main(args: Array[String]) {
        val port = Properties.envOrElse("PORT", "8080").toInt
        println("Starting on port:" + port)
        ServerBuilder()
            .codec(Http())
            .name("slackey-server")
            .bindTo(new InetSocketAddress(port))
            .build(new Slackey)
        println("Started.")
    }
}

class Slackey extends Service[HttpRequest, HttpResponse] {

    // webHook POST example
    //  token=abcdefghABCDEFGH12345678&team_id=T0001&team_domain=mydomain&service_id=1234567890&channel_id=C2147483705&channel_name=optimization&timestamp=1400726065.001431&user_id=U2147483697&user_name=bob&text=%21hello

    // slash POST example
    //  token=abcdefghABCDEFGH12345678&team_id=T0001&channel_id=C2147483705&channel_name=test&user_id=U2147483697&user_name=alice&command=/roll&text=maki

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

        if (get("token") == Web.outWebHookToken) {
            if (get("text").startsWith("%21")) {
                // trigger: starts with !
                val resp: String = "Hi " + get("user_name") + ", you said: " + Web.decode(get("text")).substring(1)
                respond(Some(resp))
            } else if (get("text").startsWith("%24")) {
                // trigger: starts with $
                val resp: String = get("user_name") + " talkin bout " + Web.decode(get("text")).substring(1) + " dollars"
                respond(Some(resp))
            } else {
                respond(None)
            }
        } else if (get("token") == Web.slashRollToken) {
            respond(dice.process(params), json = false)
        } else {
            respond(None)
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
