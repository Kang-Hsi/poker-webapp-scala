package apps.app77

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.*
import scalatags.JsDom.all.*

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("rps_text")
object TextUI extends WSClientApp:
  def appId: String = "poker"
  def uiId: String =  "text"

  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
    val _ = println("textUIInstance instancieted ...")
    TextUIInstance(userId, sendMessage, target)

class TextUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends graphics.TextClientAppInstance[Event, View](userId, sendMessage, target) {

  override def wire: AppWire[Event, View] = null // Wire

  override def handleTextInput(view: View, text: String): Option[Event] = text.toLowerCase match {
    case "fold"  => Some(Event.Fold())        // Action pour se coucher
    case "call"  => Some(Event.Check())       // Action pour suivre
    case _ => None // Ignorer les commandes non reconnues
  }

  override def renderView(userId: UserId, view11: View): Vector[TextSegment] ={
  val _ = println("RenderView started")
  val view = simulateView
  val _ = println(s"Simulated view: $view") // Vérifier si les données simulées sont générées correctement

  val title = Vector(
    TextSegment("Poker Game", cssProperties = Map("font-weight" -> "bold", "font-size" -> "20px")),
    TextSegment("\nDealer and Blinds:\n", cssProperties = Map("font-weight" -> "bold"))
  )

  println("Title segment created")

  val dealerAndBlinds = view.gameInfo.players.take(3).map { player =>
    println(s"Processing player: $player") // Vérifier chaque joueur
    val role = player._2 match {
      case Role.Dealer        => "Dealer"
      case Role.SmallBlind(_) => "Small Blind"
      case Role.BigBlind(_)   => "Big Blind"
    }
    TextSegment(s"${player._1}: $role - Balance: ${player._5}", cssProperties = Map("margin-left" -> "10px"))
  }

  val _ = println("Dealer and Blinds segment created")
  title ++ dealerAndBlinds
  }


  // Méthode pour simuler les données du backend
  private def simulateView: View = {
    val _ = println("Simulating view...")
    // Données factices pour les tests
    View(
      gameInfo = GameInfo(
        List(
          (1, Role.Dealer, Status.Playing, None, 1000),
          (2, Role.SmallBlind(50), Status.Playing, None, 500),
          (3, Role.BigBlind(100), Status.Playing, None, 200)),
        roundNumber = 1,
        communalCards = List(
          (Suit.Heart, 10, "10 Hearts"),
          (Suit.Spades, 12, "Queen Spades"),
          (Suit.Clubs, 14, "Ace Clubs")
        ),
        pot = 200,
        logs = List("Alice bet 50$", "Bob folded"),
        callAmount = 50,
        minRaise = 10,
        maxRaise = 100
      ),
      gameConfig = GameConfig(maxRound = 5)
    )
  }
}
    