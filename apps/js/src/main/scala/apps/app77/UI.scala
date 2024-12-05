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
    val rolesTable = table(
      cls := "table",
    // Ligne d'entête avec le nom de chaque joueur
      tr(
      ),
        // Lignes des données
        for ((header, rowData) <- headers.zipWithIndex) yield tr(
          td(b(header)), // Colonne de description ("Role", "Name")
          for ((name, roleOpt) <- players) yield td(
            if (rowData == 0) {
              roleOpt match
                case Role.Dealer => s"Dealer"
                case Role.SmallBlind => s"smallBlind(${view.gameConfig.smallBlind})"
                case Role.BigBlind => s"BigBlind(${view.gameConfig.bigBlind})"
                case Role.Normal => s""
          } // Ligne pour les rôles
            else name // Ligne pour les noms
            )
        )
      )

    // Actions disponibles pour le joueur
    val actions = div(
      cls := "actions",
      button(cls := "action-button", onclick := { () => sendEvent(Event.Fold()) }, "Fold"),
      button(cls := "action-button", onclick := { () => sendEvent(Event.Check()) }, "Call"),
      button(cls := "action-button", onclick := { () => sendEvent(Event.Bet(50)) }, "Raise")
    )

    // Combine tout dans une vue
    div(
      cls := "poker-ui",
      header,
      rolesTable,
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
    |   background-color: #f9f9f9;
    |   max-width: 80%; /* Limite la largeur maximale */
    |   text-align: center; /* Centre le contenu */
    | }
    |
    | .header {
    |   text-align: center;
    |   font-size: 2em;
    |   font-weight: bold;
    |   margin-bottom: 20px;
    | }
    |
    | .roles-table {
    |   margin: 20px auto;
    |   border-collapse: collapse;
    |   width: 100%; /* Adapte la largeur du tableau au conteneur */
    |   table-layout: fixed; /* Force une largeur fixe pour les colonnes */
    |   overflow-x: auto; /* Permet le défilement horizontal si nécessaire */
    |   display: block; /* Active le défilement si le tableau est trop large */
    | }
    |
    | .table th, .table td {
    |   border: 1px solid #ddd;
    |   padding: 8px;
    |   text-align: center; /* Centre les textes dans les colonnes */
    |   font-size: 1.2em; /* Augmente la taille du texte */
    | }
    |
    | .table th {
    |   background-color: #f2f2f2;
    |   font-weight: bold;
    | }
    |
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