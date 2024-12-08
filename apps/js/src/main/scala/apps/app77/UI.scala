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
    val _ = println("textUIInstance instancieted ...")
    TextUIInstance(userId, sendMessage, target)

class TextUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: Target)
    extends graphics.WebClientAppInstance[Event, View](userId, sendMessage, target) {

   // Définit le Wire utilisé pour les interactions
  override def wire: AppWire[Event, View] = apps.app77.Wire

  // Rend la vue sous forme de HTML avec ScalaTags
  override def render(userId: UserId, view: View): Frag = {
    // Header de la page
    val header = div(
      cls := "header",
      h1("Poker Game")
    )

    // Les descriptions des lignes
    val headers = List("Role", "Name","Money")
    // Tableau des joueurs et leurs rôles
    // TODO erreurs : Le sablier n'est pas sur la bonne personne (le sablier est sur la personne qui regarde le site a chaque fois)
    //                La liste ne doit pas etre dans le meme ordre que celle que vous revevez (car l'odre change a chaque tour)
    //                Il faut que vou créer une nouvelle liste ordonnée avec Dealer, Sb, Bb, et le rest (ordonné par ordre de jeu de passage de base)
    //  - il faudrait mettre une ligne betAmount (pourr voir le bet de chaque joueur
    val rolesTableContainer = div(
      cls := "roles-table-container",
      table(
        cls := "roles-table",
        for ((header, rowData) <- headers.zipWithIndex)
        yield tr(
          td(b(header)),
          for ((userId,money,role,status,_,betAmount,hasTalked,potContribution) <- view.gameInfo.players) yield td(
            if (rowData == 0) {
              role match
                case Role.Dealer => s"Dealer"
                case Role.SmallBlind => s"smallBlind(${view.gameConfig.smallBlind})"
                case Role.BigBlind => s"BigBlind(${view.gameConfig.bigBlind})"
                case Role.Normal => s""
            } else if(rowData == 1)
              if(view.gameInfo.players.filter(p => p._5.isDefined).head._1 == userId){
                frag(b(s"${userId}"),img(src := "/static/hourglass.gif", alt := "Timer", cls := "timer-icon"))
              }else{
                s"${userId}"
              }
            else{
              s"${money}$$"
            }
            )
          )
        )
    )

    // Actions disponibles pour le joueur
    // TODO A changer : le boutton call doit montrer l'argent qu'il faut payer pour call (attention a ne pas utiliser State.callAmount car ca ne marche pas, il faut utiliser la state.getCallAmount codée dans logique (copiez la fonction et adaptez la pour la view), 
    // Donc si la personne doit call, le boutton doit envoyer un BetEvent (comme convenu lundi dernier)
    // Sinon si le betAmount de al personne == callAmount, le boutton change en Check et l'on envois un checkEvent
    val actions = div(
      cls := "actions",
      button(cls := "action-button-fold", onclick := { () => sendEvent(Event.Fold()) }, "Fold"),
      button(cls := "action-button-call", onclick := { () => sendEvent(Event.Check()) }, "Call"),
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
          cls := "action-button-raise",
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
            td(
              table(
                cls := "communalTable",
                tr(
                  for card <- cards yield td(cls := "card", card)
                )
              )
            )
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
          th(cls := "tableHeader", "My money"),
          th(cls := "tableHeader", "Pot")
        ),
        tr(
          td(
            table(
              cls := "handTable",
              tr(
                for card <- myCards yield td(cls := "cardHand", card)
              )
            )
          ),
          td(cls := "tableData moneyColumn", s"${myMoney(0)} $$"),
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
        cls := "poker-ui",
        header,
        rolesTableContainer,
        commCardPot,
        playersHandBalance,
        actions,
        logs
      )
    )
  }


  // Définir le CSS pour styliser l'interface
  override def css: String = super.css + """
    | .app77 {
    |   font-family: Arial, sans-serif;
    |   margin: auto;
    |   padding: 20px;
    |   border: 1px solid #ccc;
    |   border-radius: 5px;
    |   max-width: 200%;
    |   text-align: center;
    |   justify-content: center;
    |
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
    |
    | .timer-icon {
    |   width: 50px;
    |   height: 50px;
    |   margin-left: 5px;
    |   vertical-align: middle;
    | }
    | .handTable td, .communalTable td {
    |   text-align: center;
    |   border = none;
    | }
    | .card {
    |  text-align: center; /* Centrer le contenu */
    |  display: inline-block; /* Assure un alignement horizontal */
    |  padding: 10px; /* Ajouter un espacement interne */
    |  font-size: 8em; /* Agrandir les cartes */
    |  color: #ffffff;
    | }
    | .cardHand{
    |  text-align: center; /* Centrer le contenu */
    |  display: inline-block; /* Assure un alignement horizontal */
    |  padding: 10px; /* Ajouter un espacement interne */
    |  font-size: 6.5em; /* Agrandir les cartes */
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
    | .communal-card-pot {
    |   padding: 20px;
    |   background-image: url('/static/table.jpg'); /* Chemin vers votre image */
    |   background-size: cover; /* Ajuste l'image pour couvrir tout l'arrière-plan */
    |   background-position: center; /* Centre l'image */
    |   background-repeat: no-repeat; /* Évite la répétition de l'image */
    |   border-radius: 5px; /* Suppression de la bordure */
    |   text-align: center;
    |   display: flex;
    |   justify-content: center; /* Centre le tableau horizontalement */
    |   align-items: center;
    |   box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
    |   width: 100%; /* S'assurer que le tableau prend tout l'espace disponible */
    | }
    |
    |
    | .communalTable td, .communalTable th {
    |   margin: 0 auto;
    |   padding: 8px;
    |   text-align: center;
    |   border: none; /* Conserver les bordures des cellules */
    |   width: 20%; /* Uniformiser la taille des cellules */
    | }
    | .tableHeader {
    |   border: 1px solid #ddd;
    |   padding: 8px;
    |   text-align: center;
    |   background-color: #f2f2f2;
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
    | .handTable td {
    |  text-align: center;
    |  width: auto; /* Laisser les cartes ajuster leur propre largeur */
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
