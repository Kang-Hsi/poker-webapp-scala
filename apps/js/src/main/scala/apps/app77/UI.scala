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
  def uiId: String =  "html"

  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: Target): ClientAppInstance =
    UIInstance(userId, sendMessage, target)

class UIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends graphics.WebClientAppInstance[Event, View](userId, sendMessage, target) {

   // Définit le Wire utilisé pour les interactions
  override def wire: AppWire[Event, View] = apps.app77.Wire

  // Rend la vue sous forme de HTML avec ScalaTags
  override def render(userId: UserId, view: View): Frag = {
    // Header de la page
    val header = div(
      cls := "header",
      div(cls := "layer", "Poker Game"),
      div(cls := "layer", "Poker Game")
    )

    // Les descriptions des lignes
    val headers = List("Role", "Name","Money", "Bet Amount")
    val currentplayer = view.gameInfo.players(0)._1
    val orderedListPlayers = view.gameInfo.players.sortBy(p => p._3)
    val rolesTableContainer = div(
      cls := "roles-table-container",
      table(
        cls := "roles-table",
        for ((header, rowData) <- headers.zipWithIndex)
        yield tr(
          td(b(header)),
          for ((userId,money,role,status,_,betAmount,hasTalked,potContribution) <- orderedListPlayers) yield td(
            cls := s"${if (status == Status.Spectating) "folded-player" else ""}",
            if (rowData == 0) {
              role match
                case Role.Dealer => s"Dealer"
                case Role.SmallBlind => s"smallBlind(${view.gameConfig.smallBlind}$$)"
                case Role.BigBlind => s"BigBlind(${view.gameConfig.bigBlind}$$)"
                case Role.Normal => s""
            } else if(rowData == 1)
              if(userId == currentplayer){
                frag(b(s"${userId}"),img(src := "/static/hourglass.gif", alt := "Timer", cls := "timer-icon"))
              }else{
                s"${userId}"
              }
            else if(rowData == 2) {
              s"${money}$$"
            }else{
              s"+${betAmount}$$"
            }
            )
          )
        )
    )

    val actions = div(
      cls := "actions",

      // button fold
      button(
        cls := s"action-button-fold ${if (getclient(view)._1 != view.gameInfo.players(0)._1 || getclient(view)._4 == Status.Spectating || getclient(view)._4 == Status.AllIn) "action-button-disabled" else ""}",
        onclick := { () =>
          sendEvent(Event.Fold())
        },
        "Fold"
      ),

      // button call
      button(
        cls := s"action-button-call ${if (getclient(view)._1 != view.gameInfo.players(0)._1 || getclient(view)._4 == Status.Spectating || getclient(view)._4 == Status.AllIn) "action-button-disabled" else ""}",
        onclick := { () =>
          if (getcallAmount(view) == getclient(view)._6) {
            sendEvent(Event.Check())
          } else {
            val callAmount = getcallAmount(view)
            val client = getclient(view)
            val amountToBet = math.min(client._2, callAmount - client._6)
            sendEvent(Event.Bet(amountToBet))
          }
        },
        callButtonText(view)
      ),

      // button raise
      div(
        cls := "raise-container",
        input(
          `type` := "number",
          id := "raise-input",
          placeholder := "Enter amount",
          min := s"${getcallAmount(view)}",
          cls := "raise-input"
        ),
        button(
          cls := s"action-button-raise ${if (getclient(view)._1 != view.gameInfo.players(0)._1 || getclient(view)._4 == Status.Spectating || getclient(view)._4 == Status.AllIn) "action-button-disabled" else ""}",
          onclick := { () =>
            val inputElement = dom.document
              .getElementById("raise-input")
              .asInstanceOf[HTMLInputElement]
            val raiseAmount = inputElement.value.toIntOption.getOrElse(getcallAmount(view))
            // Vérifie si la valeur est inférieure au minimum
            val correctedAmount = math.max(raiseAmount, getcallAmount(view))
            sendEvent(Event.Bet(correctedAmount))
          }
          ,
          "Raise"
        )
      )
    )

    //Comunal Card + Pot (5 cartes, parfois  certaines de dos, représenté par ?), + une colonne avec le pot
    // Récupérer les cartes communes et ajouter des cartes vides si nécessaire
    val commCards = view._1.communalCards
    val emptyCards = List.fill(5 - commCards.size)(Suit.Spades," 🂠 ")
    val cards = commCards.map(c => (c._1, c._3)) ++ emptyCards

    val commCardPot = div(
      cls := "communal-card-pot",
        table(
          cls := "communalTable",
          tr(
            td(
              table(
                cls := "communalTable",
                tr(
                  for card <- cards yield td(
                    cls := s"card ${if (card._1 == Suit.Heart|| card._1 == Suit.Diamond) "card-red" else "card-black"}", card._2)
                )
              )
            )
          )
        )
      )

    val myCards = getclient(view)._5.get.map(card => (card._1,card._3)).toList
    val myMoney = getclient(view)._2
    val playersHandBalance = div(
      table(
        tr(
          th(cls := "tableHeader", "My hand"),
          th(cls := "tableHeader", "My money"),
          th(cls := "tableHeader", "Pot")
        ),
        tr(
          td(
            table(
              cls := "handTable",
              tr(
                for card <- myCards yield td(
                  cls := s"${if (card._1 == Suit.Heart|| card._1 == Suit.Diamond) "cardHand-red" else "cardHand-black"}", card._2)
              )
            )
          ),
          td(cls := "tableData moneyColumn", s"${myMoney} $$"),
          td(
          cls := "tableData potColumn",
          div(
            cls := "pot-display",
            div(
              cls := "pot-amount",
              span(s"${view.gameInfo.pot} $$") // Montant au-dessus
            ),
            img(
              src := "/static/pot.png", // Chemin de l'image du pot
              alt := "Pot",            // Texte alternatif
              cls := "pot-image-large" // Nouvelle classe CSS pour agrandir l'image
              )
            )
          )
        )
      )
    )

    val logs = ul(
      cls := "logs",
      for log <- view.gameInfo.logs.reverse yield li(s"${log}")
    )

    // Combine tout dans une vue
    frag(
      p(
        cls := s"${if getclient(view)._1 == view.gameInfo.players(0)._1 then "poker-ui-clientTurn" else "poker-ui"}",
        header,
        rolesTableContainer,
        commCardPot,
        playersHandBalance,
        actions,
        logs
      )
    )
  }

  def getclient(view: View): PlayerInfo = {
    val client = view.gameInfo.players.filter(p => p._5.isDefined)
    require(client.size == 1)
    client(0)
  }
  private def callButtonText(view: View): String = {
    val clientMoney = getclient(view)._2
    val callAmount = view.gameInfo.callAmount

    if (clientMoney <= callAmount) {
      s"ALLIN!!"
    } else {
      if(getcallAmount(view) == getclient(view)._6){
        s"Check"
      }else{
         s"Call ${{getcallAmount(view)- getclient(view)._6}}$$"
      }
    }
  }
  private def getcallAmount(view: View):Money = {
    val players = view.gameInfo.players
    players.maxBy(p => p._6)._6
  }
  // Définir le CSS pour styliser l'interface
  override def css: String = super.css + """
    |
    | .poker-ui {
    |   font-family: Arial, sans-serif;
    |   padding: 20px;
    |   border: 3px solid #ccc;
    |   border-radius: 2px;
    |   max-width: 200%;
    |   text-align: center;
    |   justify-content: center;
    |
    | }
    | .poker-ui-clientTurn {
    |   font-family: Arial, sans-serif;
    |   margin: auto;
    |   padding: 20px;
    |   border: 3px solid #ccc;
    |   border-color: #42eb05;
    |   border-radius: 2px;
    |   max-width: 200%;
    |   text-align: center;
    |   justify-content: center;
    | }
    | .header {
    |   --c-1: #66ff66;
    |   --c-2: #e60000;
    |   position: relative;
    |   display: flex;
    |   justify-content: center;
    |   align-items: center;
    | }
    |
    | .layer {
    |   font-size: 25px;
    |   font-family: "Segoe UI", Tahoma;
    |   font-weight: 800;
    |   text-transform: uppercase;
    |   position: absolute;
    | }
    |
    | .layer:nth-child(1) {
    |   color: var(--c-1);
    |   animation: kfs-3412 2.0s infinite;
    | }
    |
    | .layer:nth-child(2) {
    |   color: var(--c-2);
    |   animation: kfs-3412 2.0s 1.0s infinite;
    | }
    |
    | @keyframes kfs-3412 {
    |   0% {
    |     text-shadow: 0 0 30px var(--c-1);
    |     transform: scaleY(0);
    |     z-index: 2;
    |   }
    |   50% {
    |     transform: scaleY(1.5);
    |     z-index: 2;
    |   }
    |   100% {
    |     transform: scaleY(1.5);
    |     z-index: 0;
    |   }
    | }
    |
    |
    | .roles-table-container {
    |   overflow-x: auto; /* Permet le défilement horizontal si nécessaire */
    | }
    |
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
    | .folded-player {
    |   opacity: 0.5; /* Réduit l'opacité pour griser */
    |   color: #888; /* Change la couleur du texte */
    |   background-color: #f0f0f0; /* Couleur de fond légèrement différente */
    | }
    |
    | .timer-icon {
    |   width: 50px;
    |   height: 50px;
    |   margin-left: 5px;
    |   vertical-align: middle;
    | }
    |
    | .actions {
    |   display: flex;
    |   justify-content: center;
    |   gap: 10px;
    |   margin-top: 20px;
    | }
    |
    | .action-button-fold {
    |   padding: 10px 20px;
    |   font-size: 1em;
    |   color: white;
    |   background-color: #fc0000;
    |   border: none;
    |   border-radius: 5px;
    |   cursor: pointer;
    | }
    | .action-button-call {
    |   padding: 10px 20px;
    |   font-size: 1em;
    |   color: white;
    |   background-color: #42cc18;
    |   border: none;
    |   border-radius: 5px;
    |   cursor: pointer;
    | }
    | .action-button-raise {
    |   padding: 10px 20px;
    |   font-size: 1em;
    |   color: white;
    |   background-color: #EFBF04;
    |   border: none;
    |   border-radius: 5px;
    |   cursor: pointer;
    | }
    |
    | .action-button:hover {
    |   background-color: #0056b3;
    | }
    |
    | .action-button-disabled {
    |  opacity: 0.5; /* Réduire l'opacité */
    |  pointer-events: none; /* Empêche tout clic ou interaction */
    |  cursor: not-allowed; /* Indique visuellement que le bouton est désactivé */
    | }
    |
    | .communal-card-pot {
    |   padding: 0px;
    |   width: 100%;
    |   height: 150px;
    |   background-image: url('/static/table.jpg');
    |   background-size: cover;
    |   background-position: 50% 55%;
    |   background-repeat: no-repeat;
    |   overflow: hidden; /* ensures anything outside the container doesn't show *
    |   border-radius: 5px; /* Suppression de la bordure */
    |   text-align: center;
    |   display: flex;
    |   justify-content: center; /* Centre le tableau horizontalement */
    |   align-items: center;
    |   width: 100%; /* S'assurer que le tableau prend tout l'espace disponible */
    |   
    | }
    |
    |
    | .communalTable td, .communalTable th {
    |   margin: 0 auto;
    |   padding: 8px;
    |   padding-top: 1.5px;
    |   text-align: center;
    |   border: none; /* Conserver les bordures des cellules */
    |   width: 20%; /* Uniformiser la taille des cellules */
    | }
    |
    | 
    | .tableHeader {
    |   border: 1px solid #ddd;
    |   padding: 8px;
    |   text-align: center;
    |   background-color: #f2f2f2;
    | }
    |
    |
    | .handTable td {
    |  width: auto; /* Laisser les cartes ajuster leur propre largeur */
    | }
    |
    | .handTable td, .communalTable td {
    |   text-align: center;
    |   border = none;
    | }
    | .card-black {
    |  text-align: center; /* Centrer le contenu */
    |  display: inline-block; /* Assure un alignement horizontal */
    |  padding: 10px; /* Ajouter un espacement interne */
    |  font-size: 8em; /* Agrandir les cartes */
    |  color: #000000;
    | }
    | .card-red {
    |  text-align: center; /* Centrer le contenu */
    |  display: inline-block; /* Assure un alignement horizontal */
    |  padding: 10px; /* Ajouter un espacement interne */
    |  font-size: 8em; /* Agrandir les cartes */
    |  color: #ff0000;
    | }
    |
    | .cardHand-black{
    |  text-align: center; /* Centrer le contenu */
    |  display: inline-block; /* Assure un alignement horizontal */
    |  padding: 10px; /* Ajouter un espacement interne */
    |  font-size: 6.5em; /* Agrandir les cartes */
    | }
    | .cardHand-red{
    |  text-align: center; /* Centrer le contenu */
    |  display: inline-block; /* Assure un alignement horizontal */
    |  padding: 10px; /* Ajouter un espacement interne */
    |  font-size: 6.5em; /* Agrandir les cartes */
    |   color: #ff0000;
    | }
    |
    | .potColumn {
    |   width: 40%; /* Largeur définie par vous */
    |   text-align: center; /* Centrer le texte */
    | }
    |
    | .tableData moneyColumn{
    |  padding: 10px; /* Espacement interne */
    |  text-align: center;
    | }
    | 
    | .pot-display {
    |   display: flex;
    |   flex-direction: column; /* Colonne pour aligner le montant au-dessus de l'image */
    |   align-items: center;    /* Centre le contenu */
    |   gap: 10px;              /* Espace entre le montant et l'image */
    | }
    |
    | .pot-amount {
    |   font-size: 1.5em; /* Augmente la taille de la police pour le montant */
    |   font-weight: bold;
    | }
    |
    | .pot-image-large {
    |   width: 150px; /* Augmente la taille de l'image */
    |   height: 150px;
    |   object-fit: contain; /* Conserve les proportions de l'image */
    | }
    |
    | .logs {
    |   margin: 10px auto;
    |   padding: 10px;
    |   width: 90%; /* Adapte la largeur des logs */
    |   max-height: 120px; /* Affiche les 4 premiers messages (approximativement) */
    |   overflow-y: scroll; /* Ajoute une barre de défilement verticale */
    |   border: 1px solid #ddd;
    |   border-radius: 5px;
    |   background-color: #f9f9f9;
    |   box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
    | }
    |
    | .logs li {
    |   padding: 5px 10px;
    |   border-bottom: 1px solid #ddd;
    |   font-size: 0.9em;
    | }
    |
    | .logs li:last-child {
    |   border-bottom: none;
    | }
    """.stripMargin
}


