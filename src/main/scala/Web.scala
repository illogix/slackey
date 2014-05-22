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
      .name("hello-server")
      .bindTo(new InetSocketAddress(port))
      .build(new Hello)
    println("Started.")
  }
}

class Hello extends Service[HttpRequest, HttpResponse] {
  def apply(req: HttpRequest) = {
    val postParams:List[String] = req.getContent.toString(Charset.forName("UTF-8")).split("&").toList
    val params = postParams.map(pp => {
      val pair = pp.split("=")
      (pair(0), pair(1))
    }).toMap

    if (params.getOrElse("text", "fail").startsWith("!")) {
      val resp: String = "Hi " + params.getOrElse("user_name", "unknown") + ", you said: " +
        params.getOrElse("text", "unknown")
      println("Sending: " + resp)
      process(Some(resp))
    } else {
      println("nothing:" + params.getOrElse("text", "blah"))
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
