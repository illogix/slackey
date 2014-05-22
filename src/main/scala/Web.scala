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
    process(req.getContent.toString(Charset.forName("UTF-8")))
  }

  def process(req: String): Future[HttpResponse] = {
    val response = Response()
    response.setStatusCode(200)
    val resp:String = "{\"text\": \"hey sup\"}"
    response.setContentString(resp)
    Future(response)
  }
}
