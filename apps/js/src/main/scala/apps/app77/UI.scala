package apps.app77

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.*
import scalatags.JsDom.all._


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
    //Comunal Card + Pot (5 cartes, parfois  certaines de dos, représenté par ?), + une colonne avec le pot
    // Récupérer les cartes communes et ajouter des cartes vides si nécessaire
    val commCards = view._1.communalCards
    val emptyCards = List.fill(5 - commCards.size)(" ? ")
    val cards = commCards.map(_._3) ++ emptyCards

    val commCardPot = div(
      cls := "communal-card-pot",
        table(
          cls := "communalTable",
          tr(
            th(cls := "tableHeader", "Communal Cards"),
            th(cls := "tableHeader", "Pot")
          ),
          tr(
            td(
              table(
                cls := "communalTable",
                tr(
                  for(card <- cards) yield td(cls := "tableData", card)
                )
              )
            ),
            td(cls := "tableData", s"${view.gameInfo.pot} $$")
          )
        )
      )

    val myCards = view.gameInfo.players.filter(p => p._5.isDefined).flatMap(p => p._5.get).map(card => card._3)
    val myMoney = view.gameInfo.players.filter(p => p._5.isDefined).map(p => p._2)
    require(myMoney.size == 1)

    val playersHandBalance = div(
      cls := "toDo",
      table(
        cls := "a-faire",
        tr(
          th(cls := "tableHeader", "My hand"),
          th(cls := "tableHeader", "My money")
        ),
        tr(
          td(
            table(
              cls := "handTable",
              tr(
                for card <- myCards yield td(cls := "tableData", card)
              )
            )
          ),
          td(cls := "tableData", s"${myMoney(0)} $$")
        )
      )
    )
    val log = div(
      
    )
    // Combine tout dans une vue
    div(
      cls := "poker-ui",
      header,
      rolesTableContainer,
      commCardPot,
      playersHandBalance,
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
    |
    |.communal-card-pot {
    |  padding: 20px;
    |  background-color: #ffffff;
    |  border: 2px solid #ccc;
    |  border-radius: 5px;
    |  box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
    |}
    |
    |.communalTable {
    |  border-collapse: collapse;
    |  width: 100%;
    |}
    |
    |.tableHeader {
    |  border: 1px solid #ddd;
    |  padding: 8px;
    |  text-align: center;
    |  background-color: #f2f2f2;
    |}
    |
    |.tableData {
    |  border: 1px solid #ddd;
    |  padding: 8px;
    |  text-align: center;
    |}
    """.stripMargin
}
