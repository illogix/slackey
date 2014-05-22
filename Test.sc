/**
 * @author sam
 *
 */
object Test {
  import java.net.URLDecoder

  val req:String = "token=REwjJhtU7rbm6fyAXt7FCni6&team_id=T02A3F3HL&team_domain=poundc&service_id=2343925644&channel_id=C02A3F3HW&channel_name=optimization&timestamp=1400726065.001431&user_id=U02A2NEUX&user_name=illo&text=%21supson&trigger_word=slackey%3Aa"



  URLDecoder.decode(req, "UTF-8")



}
