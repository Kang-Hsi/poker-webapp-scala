package apps.app77

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.*
import scalatags.JsDom.all.*

import scala.scalajs.js.annotation.JSExportTopLevel
import cs214.webapp.EventResponse.Wire

@JSExportTopLevel("app77")
object TextUI extends WSClientApp:
  def appId: String = "app77"
  def uiId: String =  "text"

  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
    val _ = println("textUIInstance instancieted ...")
    TextUIInstance(userId, sendMessage, target)

class TextUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends graphics.TextClientAppInstance[Event, View](userId, sendMessage, target) {

  override def wire: AppWire[Event, View] = apps.app77.Wire // Wire

  override def handleTextInput(view: View, text: String): Option[Event] = text.toLowerCase match {
    case "fold"  => Some(Event.Fold())        // Action pour se coucher
    case "call"  => Some(Event.Check())       // Action pour suivre
    case _ => None // Ignorer les commandes non reconnues
  }

  override def renderView(userId: UserId, view: View): Vector[TextSegment] ={
  val _ = println("RenderView started")
  val _ = println(s"Simulated view: $view") // Vérifier si les données simulées sont générées correctement

  val title = Vector(
    TextSegment("Poker Game", cssProperties = Map("font-weight" -> "bold", "font-size" -> "20px")),
    TextSegment("\nDealer and Blinds:\n", cssProperties = Map("font-weight" -> "bold"))
  )
  println("Title segment created")
  title
  }

}
    
