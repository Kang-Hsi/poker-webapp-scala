package apps.app77

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.*
import scalatags.JsDom.all.*

import scala.scalajs.js.annotation.JSExportTopLevel
import cs214.webapp.EventResponse.Wire
import org.scalajs.dom
import org.scalajs.dom.{document, HTMLInputElement}
@JSExportTopLevel("app77")
object TextUI extends WSClientApp:
  def appId: String = "app77"
  def uiId: String =  "text"

  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
    val _ = println("textUIInstance instancieted ...")
    TextUIInstance(userId, sendMessage, target)

class TextUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends graphics.WebClientAppInstance[Event, View](userId, sendMessage, target) {

   // Définit le Wire utilisé pour les interactions
  override def wire: AppWire[Event, View] = apps.app77.Wire

  // Rend la vue sous forme de HTML avec ScalaTags
  override def render(userId: UserId, view: View): Frag = {
    val players = List(
      ("Alice", Role.Dealer),
      ("Bob", Role.SmallBlind),
      ("Charlie", Role.BigBlind),
      ("Diana", Role.Normal)
    )

    // Header de la page
    val header = div(
      cls := "header",
      h1("Poker Game")
    )

    // Les descriptions des lignes
    val headers = List("Role", "Name")
    // Tableau des joueurs et leurs rôles
    val rolesTableContainer = div(
      cls := "roles-table-container",
      table(
        cls := "roles-table",
        for ((header, rowData) <- headers.zipWithIndex) 
        yield tr(
          td(b(header)),
          for ((name, roleOpt) <- players) yield td(
            if (rowData == 0) {
              roleOpt match
                case Role.Dealer => s"Dealer"
                case Role.SmallBlind => s"smallBlind(${view.gameConfig.smallBlind})"
                case Role.BigBlind => s"BigBlind(${view.gameConfig.bigBlind})"
                case Role.Normal => s""
            } else name
            )
          )
        )
    )

    // Actions disponibles pour le joueur
    val actions = div(
      cls := "actions",
      button(cls := "action-button", onclick := { () => sendEvent(Event.Fold()) }, "Fold"),
      button(cls := "action-button", onclick := { () => sendEvent(Event.Check()) }, "Call"),
      div(
        cls := "raise-container",
        input(
          `type` := "number",
          id := "raise-input",
          placeholder := "Enter amount",
          min := "10",
          cls := "raise-input"
        ),
        button(
          cls := "action-button",
          onclick := { () =>
            val inputElement = dom.document
              .getElementById("raise-input")
              .asInstanceOf[HTMLInputElement]
            val raiseAmount = inputElement.value.toIntOption.getOrElse(10)
            // Vérifie si la valeur est inférieure au min et corrige si nécessaire
            val correctedAmount = math.max(raiseAmount, 10)
            sendEvent(Event.Bet(raiseAmount))
          },
          "Raise"
        )
      )
    )

    // Combine tout dans une vue
    div(
      cls := "poker-ui",
      header,
      rolesTableContainer,
      actions
    )
  }
  

  // Définir le CSS pour styliser l'interface
  override def css: String = super.css + """
    | .poker-ui {
    |   font-family: Arial, sans-serif;
    |   margin: auto;
    |   padding: 20px;
    |   border: 1px solid #ccc;
    |   border-radius: 5px;
    |   max-width: 90%;
    |   text-align: center;
    |   box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2); /* Ajoute une ombre pour mieux délimiter */
    | }
    | .header {
    |   text-align: center;
    |   font-size: 2em;
    |   font-weight: bold;
    |   margin-bottom: 20px;
    | }
    |
    | .roles-table {
    |   margin: 20px auto;
    |   width: auto; /* Laisse la largeur s'ajuster automatiquement */
    |   border-collapse: collapse; /* Enlève les espaces entre les bordures */
    | }
    |
    | .roles-table td, .roles-table th {
    |   padding: 10px;
    |   text-align: center;
    |   border-right: 1px solid #ccc;
    |   overflow: visible;
    |   text-overflow: clip;
    |   white-space: normal; /* Permet le retour à la ligne si nécessaire */
    | }
    |
    | .roles-table td:last-child, .roles-table th:last-child {
    |   border-right: none;
    | }
    |
    | .roles-table tr {
    |   border-bottom: 1px solid #ccc;
    | }
    |
    | .roles-table th, .roles-table td {
    |   max-width: none; /* Supprime les limites de largeur */
    | }
    |
    | .roles-table-container {
    |   overflow-x: auto; /* Permet le défilement horizontal si nécessaire */
    | }
    | .actions {
    |   display: flex;
    |   justify-content: center;
    |   gap: 10px;
    |   margin-top: 20px;
    | }
    |
    | .action-button {
    |   padding: 10px 20px;
    |   font-size: 1em;
    |   color: white;
    |   background-color: #007bff;
    |   border: none;
    |   border-radius: 5px;
    |   cursor: pointer;
    | }
    |
    | .action-button:hover {
    |   background-color: #0056b3;
    | }
    """.stripMargin
}