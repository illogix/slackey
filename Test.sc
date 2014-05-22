import java.net.URLDecoder

/**
 * @author sam
 *
 */
object Test {
  val req:String = "token=REwjJhtU7rbm6fyAXt7FCni6&team_id=T02A3F3HL&team_domain=poundc&service_id=2343925644&channel_id=C02A3F3HW&channel_name=optimization&timestamp=1400726065.001431&user_id=U02A2NEUX&user_name=illo&text=%21sup%5Cteh%22son%21sup%5Cteh%22son&trigger_word=slackey%3Aa"



  val dec:String = URLDecoder.decode(req, "UTF-8").replace("\\", "\\\\").replace("\"", "\\\"")




}
