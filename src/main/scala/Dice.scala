import scala.util.Random
import scalaj.http.Http

/**
 * @author sam
 *
 */
class Dice {

  var players: Map[String, Int] = Map[String, Int]()

  val dice = new Random

  def roll(sides: Int): Int = dice.nextInt(sides) + 1

  def Dice() = {
  }

  def process(params: Map[String, String]): Option[String] = {
    def get(key: String): String = {
      params.getOrElse(key, "(unknown " + key + ")")
    }

    if (get("text").startsWith("%21start+")) {
      val playerList: List[String] = get("text").stripPrefix("%21start+").split("\\+").toList
      players = playerList.map(name => (Web.decode(name), 50)).toMap
      post("Starting game with 50 hp for the following players: " + playerList.mkString(", "))
      None
    } else if (players.keySet.contains(get("user_name"))) {
      if (players.keySet.contains(get("text"))) {
        val victim: String = get("text")
        val hp: Int = players.get(victim).get
        val dmg: Int = roll(10)
        val newHp: Int = hp - dmg
        val result: String =
          if (newHp <= 0) {
            players = players - victim
            victim + " is daed! :("
          } else {
            victim + " has " + newHp + " HP."
          }
        val resp: String = get("user_name") + " hit " + victim + " for " + dmg + ". " + result
        post(resp)
        None
      } else {
        val directMessage: String = get("text") + " isn't in the game!"
        Some(directMessage)
      }
    } else {
      val directMessage: String = "You aren't in the game!"
      Some(directMessage)
    }
  }

  def post(text: String) = {
    val postParams: List[(String, String)] = List(("username", "rollbot"), ("icon_emoji", ":game_die:"), ("text", text))
    Http.post(Web.inWebHookURL).params("payload" -> Web.makeJson(postParams)).asString
  }
}
