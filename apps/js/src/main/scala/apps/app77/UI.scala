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
    extends graphics.WebClientAppInstance[Event, View](userId, sendMessage, target){



  /**
   * Provides the application's wire implementation for communication.
   *
   * This wire connects the client-side application to the server-side logic,
   * enabling bidirectional communication. It ensures that events and views
   * are transmitted correctly between the client and server.
   * 
   * @return An instance of `AppWire` specialized for `Event` and `View` types.
   */
  override def wire: AppWire[Event, View] = apps.app77.Wire

  /**
   * Renders the view based on the current game phase.
   * 
   * This method dynamically determines the appropriate interface to render 
   * depending on the game's phase. It supports three main phases:
   * - `EndGame`: Displays the final results and the winner(s) of the game.
   * - `EndRound`: Shows a temporary interface highlighting the round's winner.
   * - Other phases: Renders the interface for an ongoing round.
   * 
   * @param userId The identifier of the current user (client).
   * @param view The current state of the game, including phase and player information.
   * @return A `Frag` representing the rendered interface for the current game phase.
   */
  override def render(userId : UserId, view: View): Frag = {
    view.gamePhase match
      case GamePhase.EndGame => endGameRender(view)
      case GamePhase.EndRound => endRoundRender(view)
      case _ => roundRender(userId, view) 
  }

  /**
   * Renders the temporary interface for the end of a round.
   * 
   * Extracts the round winner's name and the amount won from the game logs.
   * Displays a temporary styled interface before re-rendering the game view.
   *
   * @param view The current game state containing logs and game details.
   * @return A `Frag` representing the temporary end-round interface.
   */
  private def endRoundRender(view : View) = {
    val message = view.gameInfo.logs.last
    val messageParts = message.split(" ")
    val winnerName = messageParts(0)
    val amountWon = messageParts(2)
    val tempInterface = div(
      cls := "end-round-container",
      h2(cls := "end-round-title", "🏆 Round Winner 🏆"),
      div(
        cls := "winner-info",
        p(
          cls := "winner-name",
          span("Winner: "), 
          span(cls := "highlight", s"$winnerName")
        ),
        p(
          cls := "winner-amount",
          span("Amount Won: "), 
          span(cls := "highlight", s"$amountWon")
        )
      )
    )
    dom.window.setTimeout(() => { render(userId, view)}, 3000) // Re-render after 3 seconds
    tempInterface
  }

  /**
   * Renders the interface for the end of the game.
   * 
   * Determines the player(s) with the highest total money from the game state 
   * and displays their names and total winnings in a styled, congratulatory view.
   * 
   * @param view The current game state containing player information and their money amounts.
   * @return A `Frag` representing the end-game interface with the winner(s) and total money.
   */
  private def endGameRender(view: View) = {
    // Sort players by their total money in descending order
    val orderedListPlayersbyAmount = view.gameInfo.players.sortBy(-_._2)

    // Get the highest amount and determine all winners with that amount
    val highestAmount = orderedListPlayersbyAmount.head._2
    val winners = view.gameInfo.players
      .filter(p => p._2 == highestAmount)
      .map(_._1)
      .mkString(", ")

    // Construct the end-game interface
    div(
      cls := "winner-container-endgame",
      h1(cls := "winner-title-endgame", "🎉 Congratulations! 🎉"), 
      div(
        cls := "winner-info-endgame",
        h2(cls := "winner-name-endgame", s"Winner(s): $winners"),
        p(cls := "winner-money-endgame", s"Total Money: ${highestAmount} $$"),
        p(cls := "winner-message-endgame", "You played like a champion! 🏆")
      ),
      button(
        cls := "play-again-button",
        onclick := { () => sendEvent(Event.Restart()) },
        "Play Again"
      )
    )
  }

  /**
   * Renders the interface for a single round of the game.
   * 
   * Displays the round number, game header, player roles, communal cards, 
   * player's hand, actions, and logs. This method dynamically applies styles 
   * to indicate the current player's turn.
   * 
   * @param userId The identifier of the current user (client).
   * @param view The current state of the game containing player and round information.
   * @return A `Frag` that combines all components of the round interface.
   */
  private def roundRender(userId: UserId, view: View): Frag = {
    // Round number view
    val round = div(
      b(s"Rounds: ${view.gameInfo.roundNumber} / 15")
    )
    // Header with animated "Poker Game" text
    val header = div(
      cls := "header",
      div(cls := "layer", "Poker Game"),
      div(cls := "layer", "Poker Game")
    )
    // Table with player roles and general game info
    val rolesTableContainer = generalTableInformationsRender(view)

    // Communal cards display
    val commCard = communalCardsRender(view)

    // Action buttons (Fold, Call, Raise)
    val actions = buttonsActionRender(view)

    // Player's hand and balance
    val playersHandBalance = playersHandBalanceRender(view)

    // Game logs
    val logs = logsRender(view)

    // Combine all the components into a unified interface
    frag(
      round,
      p(
        cls := s"${if getclient(view)._1 == view.gameInfo.players(0)._1 then "poker-ui-clientTurn" else "poker-ui"}",
        header,
        rolesTableContainer,
        commCard,
        playersHandBalance,
        actions,
        logs
      )
    )
  }


  /**
   * Retrieves the client player's information from the game state.
   * 
   * If the client is no longer in the game (e.g., has lost), assigns a default
   * empty hand to the client and returns the player's information.
   * 
   * @param view The current game state containing the list of players.
   * @return The client player's information as a `PlayerInfo`.
   */
  private def getclient(view: View): PlayerInfo = {
    // Filter players to find the client based on their hand
    val client = view.gameInfo.players.filter(p => p._5.isDefined)
    val newPlayerHand = Set(new Card(Suit.Heart, 0, " 🂠 "), new Card(Suit.Heart, 0, "🂠"))
    // If no client is found, assign an empty hand to the first player with no money
    if (client.isEmpty) {
      view.gameInfo.players
        .filter(p => p._2 == 0)(0)
        .copy(_5 = newPlayerHand)
    }
    // Return the client information
    client(0)
  }

  /**
   * Generates the text for the "Call" button based on the game state.
   * 
   * Determines the appropriate action label ("ALLIN!!", "Check", or "Call <amount>$")
   * by comparing the client's current money, call amount, and bet amount.
   * 
   * @param view The current game state containing player and game details.
   * @return A `String` representing the text to display on the "Call" button.
   */
  private def callButtonText(view: View): String = {
    // Retrieve the client's money and the current call amount
    val clientMoney = getclient(view)._2
    val callAmount = view.gameInfo.callAmount

    // If the client's money is less than or equal to the call amount, display "ALLIN!!"
    if (clientMoney <= callAmount) {
      s"ALLIN!!"
    } else {
      // If the client has already matched the call amount, display "Check"
      if(getcallAmount(view) == getclient(view)._6){
        s"Check"
      } else {
        // Otherwise, calculate the remaining call amount
        s"Call ${{getcallAmount(view) - getclient(view)._6}}$$"
      }
    }
  }

  /**
   * Retrieves the highest call amount placed by any player in the current round.
   * 
   * This method examines all players in the game to find the maximum bet amount
   * contributed during the ongoing round. It ensures the correct amount required
   * for the current player to match or exceed to stay in the round.
   * 
   * @param view The current game state containing player details and contributions.
   * @return The highest call amount (`Money`) in the current game state.
   */
  private def getcallAmount(view: View): Money = {
    // Extract the list of players from the game state
    val players = view.gameInfo.players

    // Determine the highest bet amount among all players
    players.maxBy(p => p._6)._6
  }

  /**
   * Renders the general information table for the game, including player roles, names,
   * money, and bet amounts.
   * 
   * This method creates an HTML table that displays the relevant details for all players.
   * It uses a dynamic approach to assign CSS classes for styling based on the player's
   * status and highlights the current player with additional styling.
   * 
   * @param view The current game state containing player information and configurations.
   * @return A `Frag` representing the table of player details.
   */
  private def generalTableInformationsRender(view: View) = {
    val headers = List("Role", "Name", "Money", "Bet Amount") // Table headers
    val currentplayer = view.gameInfo.players(0)._1          // Identifies the current player
    val orderedListPlayers = view.gameInfo.players.sortBy(p => p._3) // Orders players by role

    // Constructs the table structure
    div(
      cls := "roles-table-container",
      table(
        cls := "roles-table",
        for ((header, rowData) <- headers.zipWithIndex) // Iterates through each header
        yield tr(
          td(b(header)), // Displays the header in bold
          for ((userId, money, role, status, _, betAmount, hasTalked, potContribution) <- orderedListPlayers) 
          yield td(
            cls := s"${if (status == Status.Spectating) "folded-player" else ""}", // Applies CSS class for spectating players
            if (rowData == 0) { // Handles role-specific rendering
              role match
                case Role.Dealer => s"Dealer"
                case Role.SmallBlind => s"smallBlind(${view.gameConfig.smallBlind}$$)"
                case Role.BigBlind => s"BigBlind(${view.gameConfig.bigBlind}$$)"
                case Role.Normal => s""
            } else if (rowData == 1) { // Handles name rendering
              if (userId == currentplayer) {
                if (userId == getclient(view)._1)
                  td(
                    div(
                      span(cls := "currentPlayer-name", s"${userId}"),
                      span(img(src := "/static/hourglass.gif", alt := "Timer", cls := "timer-icon"))
                    )
                  )
                else frag(b(s"${userId}"), img(src := "/static/hourglass.gif", alt := "Timer", cls := "timer-icon"))
              } else if (userId == getclient(view)._1) {
                div(cls := "currentPlayer-name", s"${userId}")
              } else {
                s"${userId}"
              }
            } else if (rowData == 2) { // Handles money rendering
              s"${money}$$"
            } else { // Handles bet amount rendering
              s"+${betAmount}$$"
            }
          )
        )
      )
    )
  }

  /**
   * Renders the game logs as a list of messages.
   * 
   * This method displays the game logs in reverse order, with the most recent log
   * message appearing at the top. The logs are styled as a scrollable list to accommodate
   * longer game histories.
   * 
   * @param view The current game state containing the list of log messages.
   * @return A `Frag` representing the list of game logs displayed in a scrollable container.
   */
  private def logsRender(view: View) = {
    // Creates an unordered list (ul) element to display the logs
    ul(
      cls := "logs", // CSS class for styling the logs container
      for log <- view.gameInfo.logs.reverse yield li(s"${log}") // Iterates through logs in reverse order
    )
  }

  /**
   * Renders the action buttons (Fold, Call, Raise) for the player.
   * 
   * This method dynamically generates buttons based on the current game state. 
   * It ensures the appropriate button states (enabled/disabled) and actions 
   * depending on the player's turn, status, and available options.
   * 
   * @param view The current game state containing player and action details.
   * @return A `Frag` containing the action buttons as HTML elements.
   */
  private def buttonsActionRender(view: View) = {
    div(
      cls := "actions",

      // Fold button
      button(
        cls := s"action-button-fold ${if (getclient(view)._1 != view.gameInfo.players(0)._1 || getclient(view)._4 == Status.Spectating || getclient(view)._4 == Status.AllIn) "action-button-disabled" else ""}",
        onclick := { () =>
          sendEvent(Event.Fold())
        },
        "Fold"
      ),

      // Call button
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
        callButtonText(view) // Display the appropriate text for the call button
      ),

      // Raise input and button
      div(
        cls := "raise-container",
        input(
          `type` := "number",
          id := "raise-input",
          placeholder := "Enter amount",
          min := s"${getcallAmount(view)}", // Minimum value is the current call amount
          cls := "raise-input"
        ),
        button(
          cls := s"action-button-raise ${if (getclient(view)._1 != view.gameInfo.players(0)._1 || getclient(view)._4 == Status.Spectating || getclient(view)._4 == Status.AllIn) "action-button-disabled" else ""}",
          onclick := { () =>
            val inputElement = dom.document
              .getElementById("raise-input")
              .asInstanceOf[HTMLInputElement]
            val raiseAmount = inputElement.value.toIntOption.getOrElse(getcallAmount(view))
            val correctedAmount = math.max(raiseAmount, getcallAmount(view))
            sendEvent(Event.Bet(correctedAmount))
          },
          "Raise"
        )
      )
    )
  }

  /**
   * Renders the player's hand and balance, along with the communal pot.
   * 
   * Displays the cards in the player's hand, their current money balance, 
   * and the communal pot in a well-structured table format.
   * The cards are styled differently based on their suit (red for Hearts/Diamonds, black for Spades/Clubs).
   * 
   * @param view The current game state containing the player's information and communal pot.
   * @return A `Frag` containing the player's hand, balance, and pot as a table.
   */
  private def playersHandBalanceRender(view: View) = {
    val myCards = getclient(view)._5.get.map(card => (card._1, card._3)).toList // Extract player's cards
    val myMoney = getclient(view)._2 // Extract player's money

    // Build the table for the player's hand, money, and communal pot
    div(
      table(
        // Header row
        tr(
          th(cls := "tableHeader", "My hand"),
          th(cls := "tableHeader", "My money"),
          th(cls := "tableHeader", "Pot")
        ),
        // Data row
        tr(
          td(
            table(
              cls := "handTable",
              tr(
                // Render each card with appropriate styling
                for card <- myCards yield td(
                  cls := s"${if (card._1 == Suit.Heart || card._1 == Suit.Diamond) "cardHand-red" else "cardHand-black"}",
                  card._2
                )
              )
            )
          ),
          // Display player's money
          td(cls := "tableData moneyColumn", s"${myMoney} $$"),
          // Display the communal pot with an image
          td(
            cls := "tableData potColumn",
            div(
              cls := "pot-display",
              div(
                cls := "pot-amount",
                span(s"${view.gameInfo.pot} $$")
              ),
              img(
                src := "/static/pot.png",
                alt := "Pot",
                cls := "pot-image-large"
              )
            )
          )
        )
      )
    )
  }

  /**
   * Renders the communal cards on the table.
   * 
   * This method displays the cards currently shared among all players in the round.
   * Cards are styled based on their suit: red for Hearts and Diamonds, and black for Spades and Clubs.
   * Empty card slots (up to 5 total communal cards) are filled with placeholder cards.
   * 
   * @param view The current game state containing communal cards information.
   * @return A `Frag` representing the communal cards display.
   */
  private def communalCardsRender(view: View) = {
    // Retrieve the list of communal cards from the game state
    val commCards = view._1.communalCards
    
    // Create placeholder cards to fill up to 5 communal cards
    val emptyCards = List.fill(5 - commCards.size)((Suit.Spades, " 🂠 "))
    
    // Combine existing communal cards with placeholders
    val cards = commCards.map(c => (c._1, c._3)) ++ emptyCards

    // Render the communal cards as a styled container
    div(
      cls := "communal-card-pot",
      for card <- cards yield div(
        cls := "card-container", // Style for each card container
        div(
          cls := s"card-icon ${if (card._1 == Suit.Heart || card._1 == Suit.Diamond) "card-red" else "card-black"}", // Apply styling based on suit
          card._2 // Card icon or placeholder
        )
      )
    )
  }
  /**
   * Combines the base CSS styles with custom Poker Game interface styling.
   * 
   * This method overrides the base CSS definitions with additional styles specific
   * to the Poker Game interface. These styles include animations, table layouts,
   * player indicators, communal card displays, and action buttons to provide a
   * rich user experience tailored for the game.
   * 
   * The following features are defined:
   * - Styling for the poker game layout and player interactions.
   * - Animations for highlighting the active player's turn.
   * - Table designs for displaying roles, player information, and game details.
   * - Styling for action buttons (Fold, Call, Raise) based on player state.
   * - Visual indicators for communal cards, player hand, and game logs.
   * - Special interface for end-round and end-game displays.
   * 
   * @return A string containing all the CSS styles combined with the base styles.
   */
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
    |  font-family: Arial, sans-serif;
    |  margin: auto;
    |  padding: 20px;
    |  border: 3px solid #ccc;
    |  border-color: #42eb05; /* Static border color */
    |  border-radius: 2px;
    |  max-width: 200%;
    |  text-align: center;
    |  justify-content: center;
    |  animation: pulse-border 2s infinite; /* Animation for the green line */
    | }
    |
    | @keyframes pulse-border {
    |  0% {
    |    border-color: #42eb05; /* Start with the green color */
    |    box-shadow: 0 0 5px #42eb05;
    |  }
    |  50% {
    |    border-color: #7fff00; /* Lighter green for pulse */
    |    box-shadow: 0 0 15px #7fff00;
    |  }
    |  100% {
    |    border-color: #42eb05; /* Return to the original green */
    |    box-shadow: 0 0 5px #42eb05;
    |  }
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
    |   overflow-x: auto;
    | }
    |
    |
    | .roles-table {
    |   margin: 20px auto;
    |   width: auto;
    |   border-collapse: collapse;
    | }
    |
    | .roles-table td, .roles-table th {
    |   padding: 10px;
    |   text-align: center;
    |   border-right: 1px solid #ccc;
    |   overflow: visible;
    |   text-overflow: clip;
    |   white-space: normal;
    |   max-width: none;
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
    |
    | .folded-player {
    |   opacity: 0.5;
    |   color: #888;
    |   background-color: #f0f0f0;
    | }
    | .currentPlayer-name {
    |   color: #F92C00;
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
    |  opacity: 0.5;
    |  pointer-events: none;
    |  cursor: not-allowed;
    | }
    |
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
    | .communal-card-pot {
    |  display: flex;
    |  justify-content: center;
    |  align-items: center;
    |  padding: 10px;
    |  background-color: white;
    |  border-radius: 5px;
    |  overflow: hidden;
    | }
    |
    | .card-container {
    |  display: flex;
    |  justify-content: center; 
    |  align-items: center;
    |  background-color: white;
    |  border-radius: 5px;
    |  margin: 5px;
    |  overflow: hidden;
    | }
    |
    | .card-icon {
    |  display: flex;
    |  justify-content: center;
    |  align-items: center;
    |  width: 100%;
    |  height: 100%;
    |  font-size: 7rem;
    |  line-height: 1;
    |  overflow: hidden;
    |  margin: 0;
    |  padding: 0;
    |  border: none;
    | }
    | .card-black {
    |  background-color: #FFFFFF;
    |  text-align: center;
    |  display: flex;
    |  justify-content: center;
    |  align-items: center;
    |  width: 100%;
    |  height: 100%;
    |  font-size: 6rem;
    |  color: #000000;
    | }
    |
    | .card-red {
    |  background-color: #FFFFFF;
    |  text-align: center;
    |  display: flex;
    |  justify-content: center;
    |  align-items: center;
    |  width: 100%;
    |  height: 100%;
    |  font-size: 6rem;
    |  color: #FF0000;
    | }
    |
    | .cardHand-black{
    |  text-align: center;
    |  display: inline-block;
    |  padding: 10px; 
    |  font-size: 6.5em; 
    | }
    | .cardHand-red{
    |  text-align: center;
    |  display: inline-block;
    |  padding: 10px; 
    |  font-size: 6.5em;
    |   color: #ff0000;
    | }
    |
    | .potColumn {
    |   width: 40%;
    |   text-align: center;
    | }
    |
    | .tableData moneyColumn{
    |  padding: 10px; /* Espacement interne */
    |  text-align: center;
    | }
    |
    | .pot-display {
    |   display: flex;
    |   flex-direction: column;
    |   align-items: center;
    |   gap: 10px;
    | }
    |
    | .pot-amount {
    |   font-size: 1.5em;
    |   font-weight: bold;
    | }
    |
    | .pot-image-large {
    |   width: 120px;
    |   height: 120px;
    |   object-fit: contain;
    | }
    |
    | .logs {
    |   margin: 10px auto;
    |   padding: 10px;
    |   width: 90%;
    |   max-height: 120px; /* print the firsts 4 messages */
    |   overflow-y: scroll;
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
    |
    | .end-round-container {
    |  text-align: center;
    |  padding: 20px;
    |  background-color: #fffae6;
    |  border: 3px solid #ffcc00;
    |  border-radius: 10px;
    |  box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.2);
    |  margin: 20px auto;
    |  max-width: 400px;
    |  animation: fadeInOut 5s ease-in-out;
    | }
    |
    | .end-round-title {
    |  font-size: 2em;
    |  color: #333;
    |  margin-bottom: 15px;
    | }
    |
    | .winner-info {
    |  font-size: 1.2em;
    |  margin-bottom: 10px;
    | }
    |
    | .highlight {
    |  font-weight: bold;
    |  color: #ff9900;
    | }
    |
    | @keyframes fadeInOut {
    |  0% { opacity: 0; }
    |  20% { opacity: 1; }
    |  80% { opacity: 1; }
    |  100% { opacity: 0; }
    | }
    |
    | .winner-container-endgame {
    |  text-align: center;
    |  padding: 30px;
    |  background: linear-gradient(to bottom, #fffbe6, #ffefd1);
    |  border: 4px solid #ffcc00;
    |  border-radius: 12px;
    |  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.2);
    |  margin: 30px auto;
    |  max-width: 500px;
    |  animation: fadeInSlide 1s ease-in-out;
    | }
    |
    | .winner-title-endgame {
    |  font-size: 2.5em;
    |  font-weight: bold;
    |  color: #333;
    |  margin-bottom: 20px;
    |  text-shadow: 2px 2px 4px rgba(255, 204, 0, 0.6);
    | }
    |
    | .winner-info-endgame {
    |  font-size: 1.4em;
    |  color: #555;
    |  margin: 10px 0;
    | }
    |
    | .winner-name-endgame {
    |  font-weight: bold;
    |  color: #ff9900;
    |  font-size: 1.8em;
    |  text-shadow: 1px 1px 3px rgba(0, 0, 0, 0.2);
    | }
    |
    | .winner-money-endgame {
    |  font-size: 1.6em;
    |  color: #2ecc71;
    |  font-weight: bold;
    | }
    |
    | .winner-message-endgame {
    |  font-size: 1.1em;
    |  color: #666;
    |  font-style: italic;
    |  margin-top: 10px;
    | }
    |
    | .play-again-button {
    |  margin-top: 20px;
    |  padding: 12px 25px;
    |  font-size: 1.1em;
    |  color: white;
    |  background: linear-gradient(to right, #ffcc00, #ffa500);
    |  border: none;
    |  border-radius: 8px;
    |  cursor: pointer;
    |  font-weight: bold;
    |  text-transform: uppercase;
    |  box-shadow: 0 3px 6px rgba(0, 0, 0, 0.2);
    |  transition: all 0.3s ease;
    | }
    |
    | .play-again-button:hover {
    |  background: linear-gradient(to right, #ffa500, #ffcc00);
    |  box-shadow: 0 5px 10px rgba(0, 0, 0, 0.3);
    |  transform: scale(1.05);
    | }
    |
    | @keyframes fadeInSlide {
    |  from {
    |    opacity: 0;
    |    transform: translateY(-30px);
    |  }
    |  to {
    |    opacity: 1;
    |    transform: translateY(0);
    |  }
    | }
    """.stripMargin
}