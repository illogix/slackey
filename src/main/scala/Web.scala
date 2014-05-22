import java.nio.charset.Charset
import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse}
import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.{Http, Response}
import com.twitter.finagle.Service
import com.twitter.util.Future
import java.net.InetSocketAddress
import util.Properties
import scalaj.http.{Http => Jhttp}

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
  def apply(req: HttpRequest) = {
    val postParams:List[String] = req.getContent.toString(Charset.forName("UTF-8")).split("&").toList
    val params = postParams.map(pp => {
      val pair = pp.split("=")
      (pair(0), pair(1))
    }).toMap

    if (params.getOrElse("text", "").startsWith("%21")) {
      val resp: String = "Hi " + params.getOrElse("user_name", "unknown") + ", you said: " +
        params.getOrElse("text", "unknown")
      process(Some(resp))
    } else {
      process(None)
    }
  }

  def process(resp:Option[String]): Future[HttpResponse] = {
    val response = Response()
    response.setStatusCode(200)
    resp match {
      case Some(text) => response.setContentString("{\"text\": \"" + text + "\"}")
      case None =>
    }
    Future(response)
  }
}
