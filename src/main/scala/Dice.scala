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

    def fight(a: String, b: String): String = {
      attack(a, b) + checkLife(a) + ", " + checkLife(b) + checkWin()
    }

    def attack(a: String, b: String): String = {
      hit(a)
      hit(b)
      a + ":sword:" + b + "! "
    }

    def hit(player: String) = {
      val hp: Int = players.get(player).get
      val dmg: Int = roll(10)
      val newHp: Int = hp - dmg
      players = players + ((player, newHp))
    }

    def checkLife(player: String): String = {
      val hp: Int = players.get(player).get
      if (hp <= 0) {
        players = players - player
        player + ": :skull:"
      } else {
        player + ": " + hp + "HP"
      }
    }

    def checkWin(): String = {
      if (players.size == 1) {
        val grats:String = "\n" + players.keysIterator.next() + ":trophy::thumbsup:"
        players = players.empty
        grats
      } else {
        ""
      }
    }

    if (get("text").startsWith("%21start+")) {
      val playerList: List[String] = get("text").stripPrefix("%21start+").split("\\+").toList
      players = playerList.map(name => (Web.decode(name), 50)).toMap
      post("Starting game with 50 HP for the following players: " + playerList.mkString(", "))
      None
    } else if (players.keySet.contains(get("user_name"))) {
      if (players.keySet.contains(get("text"))) {
        val attacker: String = get("user_name")
        val defender: String = get("text")
        val result: String = fight(attacker, defender)

        post(result)
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
