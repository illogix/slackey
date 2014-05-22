import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.{Http, Response}
import com.twitter.finagle.Service
import com.twitter.util.Future

import java.net.InetSocketAddress
import java.net.URLDecoder
import java.nio.charset.Charset

import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse}

import scalaj.http.{Http => HttpJ}

import util.Properties

object Web {
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

  // Tokens
  val outWebHookToken: String = "REwjJhtU7rbm6fyAXt7FCni6"
  val slashRollToken: String = "b731yQBiwC3cwg24IlGjiEXS"
  val inWebHookToken: String = "rFCX9chyUckjizCRPeuAwryX"

  // URLs
  val inWebHookURL: String = "https://poundc.slack.com/services/hooks/incoming-webhook?token="

  // webHook POST example
  //  token=REwjJhtU7rbm6fyAXt7FCni6&team_id=T02A3F3HL&team_domain=poundc&service_id=2343925644&
  //  channel_id=C02A3F3HW&channel_name=optimization&timestamp=1400726065.001431&user_id=U02A2NEUX&
  //  user_name=illo&text=%21sup%5Cteh%22son&trigger_word=slackey%3Aa

  // slash POST example
  //  token=b731yQBiwC3cwg24IlGjiEXS
  //  team_id=T0001
  //  channel_id=C2147483705
  //  channel_name=test
  //  user_id=U2147483697
  //  user_name=Steve
  //  command=/weather
  //  text=94070

  val dice: Dice = new Dice

  def apply(req: HttpRequest): Future[HttpResponse] = {
    val postParams: List[String] = req.getContent.toString(Charset.forName("UTF-8")).split("&").toList
    val params = postParams.map(pp => {
      val pair = pp.split("=")
      (pair(0), pair(1))
    }).toMap
    process(params)
  }

  def convert(urlEncoded: String): String = {
    URLDecoder.decode(urlEncoded, "UTF-8")
  }

  def makeJson(key: String, value: String): String = {
    "{\"" + key + "\": \"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\"}"
  }

  def process(params: Map[String, String]): Future[HttpResponse] = {
    def get(key: String): String = {
      params.getOrElse(key, "(unknown " + key + ")")
    }

    if (get("token") == outWebHookToken) {
      if (get("text").startsWith("%21")) {
        // trigger: starts with !
        val resp: String = "Hi " + get("user_name") + ", you said: " + convert(get("text")).substring(1)
        respond(Some(resp))
      } else if (get("text").startsWith("%24")) {
        // trigger: starts with $
        val resp: String = get("user_name") + " talkin bout " + convert(get("text")).substring(1) + " dollars"
        respond(Some(resp))
      } else {
        respond(None)
      }
    } else if (get("token") == slashRollToken) {
      val resp: String = get("user_name") + " rolling with: " + convert(get("text"))
      HttpJ.post(inWebHookURL).params("text" -> makeJson("text", resp))
      respond(None)
    } else {
      respond(None)
    }
  }

  def respond(resp: Option[String]): Future[HttpResponse] = {
    val response = Response()
    response.setStatusCode(200)
    resp match {
      case Some(respText) => response.setContentString(makeJson("text", respText))
      case None =>
    }
    Future(response)
  }
}
