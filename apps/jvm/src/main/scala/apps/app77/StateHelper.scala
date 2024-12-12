package apps.app77

import apps.app77.CardHelper.shuffle
import apps.app77.Logic
import apps.app77.GamePhase.*
import apps.app77.Role.*
import cs214.webapp.*

extension (state: State)

  /** Returns state with new deck shuffled.
    *
    * @return
    *   state with new deck shuffled.
    */
  def populateShuffledDeck(): State =
    state.copy(deck = CardHelper.allCards.shuffle())

  /** Returns a state with cards distributed to each player. Makes sure to only
    * distribute cards to players that are playing.
    *
    * @return
    *   state with cards distributed.
    */
  def distributeCards(): State =
    val deckIterator = state.deck.iterator
    val gameInfo = state.gameInfo
    val players = gameInfo.players

    val playersWithCards =
      (for {
        player <- players
        if (player.isPlaying())
      } yield player.withOptionHand(
        Some(
          Set(deckIterator.next(), deckIterator.next())
        )
      )).toList

    val gameInfoUpdated = gameInfo.copy(players = playersWithCards)
    state.copy(
      deck = deckIterator.toList,
      gameInfo = gameInfoUpdated
    )

  /** Returns state with players turn rotated.
    *
    * @return
    *   state with players turn rotated.
    */
  def rotatePlayerTurn(): State =
    if (state.gameInfo.players.count(player => player.isOnlyPlaying()) <= 1 && hasEveryoneTalked) || state.gameInfo.players.filter(_.getStatus() != Status.Spectating).forall(_.isOnlyAllIn()) then 
      println("DEBUG: ROTATING PLAYERS")
      state else
      println("DEBUG: ROTATING PLAYERS 2")
      val newRotatedState = state.copy(gameInfo = state.gameInfo.rotatePlayerTurnInternal())
      print("boucle") 
      if !newRotatedState.gameInfo.players.head.isOnlyPlaying() then
        newRotatedState.rotatePlayerTurn()
      else
        newRotatedState

  /** Returns state with the role of the players rotated. This function is
    * implemented in a "hard way". Since we have to assume it could be called
    * anytime.
    *
    * So this functions rotates the players roles BUT does not rotate to the
    * next player. Sets the amount of BigBlind & SmallBlind to 0.
    *
    * @return
    *   state with players roles rotated.
    */
  def rotatePlayerRole(): State =
    state.copy(gameInfo = state.gameInfo.rotatePlayerRolesInternal())

  /** Returns state with the order of the round set. PreFlop: Player after big
    * blinds starts. Flop, turn, river: Small Blind starts.
    *
    * @return
    *   state with the order of the round set.
    */
  def setBeginOfRoundOrder(): State =
    val stateWithOrder = state.copy(gameInfo = state.gameInfo.setBeginOfRoundOrderInternal(state))
    
    if !stateWithOrder.gameInfo.players.head.isOnlyPlaying() then
      stateWithOrder.rotatePlayerTurn()
    else
      stateWithOrder

  /** Returns state with the blinds populated (depending on the game's
    * configuration).
    *
    * @return
    *   state with the blinds populated.
    */
  def populateBlinds(using Configuration): State =
    val gameConfigWithBlinds = state.gameConfig
      .copy(
        smallBlind = configuration.getSmallBlind,
        bigBlind = configuration.getBigBlind
      )

    state.copy(gameConfig = gameConfigWithBlinds)

  /** Returns state with next phase (if possible).
    *
    * @return
    *   state with next phase.
    *
    * @throws IllegalArgumentExcecption
    *   if current phase is end game.
    */
  def nextPhase(): State =
    state.gamePhase match
      case PreFlop  => state.copy(gamePhase = Flop)
      case Flop     => state.copy(gamePhase = Turn)
      case Turn     => state.copy(gamePhase = River)
      case River    => state.copy(gamePhase = EndRound)
      case EndRound => state.copy(gamePhase = PreFlop)
      case EndGame =>
        throw IllegalArgumentException("Game has ended! No nextPhase")

  /** Returns state with an event applied (naively) updating only the money, the
    * pot and status.
    *
    * @param user
    *   a user (player) id.
    * @param event
    *   an event.
    * @return
    *   state with an event applied.
    *
    * @throws IllegalArgumentException
    *   if user id is not valid.
    * @throws IllegalMoveException
    *   if player is not playing, or if it's not his turn.
    */
  def applyEventNaive(user: UserId, event: Event): State =
    val players = state.gameInfo.players

    val userIndex = players.indexWhere(_.getUserId() == user)

    require(userIndex != -1, "Unknown user!")

    val player = players(userIndex)

    // if player is all in or spectacting, cannot do anything
    if !player.isOnlyPlaying() then
      throw IllegalMoveException(
        "You are spectating / Alled in, you cannot do any action"
      )

    // the user is ensured to be playing

    // the player must then be its turn:

    if userIndex != 0 then throw IllegalMoveException("please wait your turn")

    println("DEBUG: Player doing action: " + player)
    event match
      case Event.Fold() =>
        val allPlaying = state.gameInfo.getAllPlayingPlayers
        if allPlaying.length == 1 then
          throw IllegalMoveException(
            "You cannot fold if you are the last one playing / all'd in"
          )

        state.applyFold(user, userIndex)

      case Event.Check() =>
        // can only check if everyone one has checked / or no one has bet
        if state.getCallAmount() != player.getBetAmount() then  
          throw IllegalMoveException(
            "You cannot check, as you need to to call/raise!"
          )

        println("DEBUG: " + " bet Amount: " + player.getBetAmount())

        state.applyCheck(user, userIndex)

      case Event.Bet(amount) =>
        require(amount > 0, "cannot bet <= 0")

        
        // shouldn't happen though
        if player.getMoney() < amount then
          throw IllegalMoveException("Not enough money")

        val totalBet = player.getBetAmount() + amount
        val callAmount = state.getCallAmount()

        println("DEBUG: " + user + " callAmount = " + callAmount)
        println("DEBUG: " + user + " old bet amount " + player.getBetAmount() + " and new bet amount: " + totalBet)
        println("DEBUG: Bet amount is : " + amount)
        // player is calling
        if totalBet == callAmount then
          if player.getMoney() == amount then
            println("PLAYER ALL IN JSEHFJHSLDJFKLJSDF")
            state.applyAllIn(user, userIndex, amount, false) else
          state.applyBet(user, userIndex, amount, Status.Playing, false)
        else if totalBet < callAmount then
          // player is calling but does not have enough money => all-in
          if player.getMoney() == amount then
            state.applyAllIn(user, userIndex, amount, false)
          else
            // should not happen (on the UI), can only bet what you have
            throw IllegalMoveException("Not enough to call, not an all in")
        else
          // player is raising
          val playerIsRaising = totalBet > callAmount

          // player is all in
          if player.getMoney() == amount then
            state.applyAllIn(user, userIndex, amount, playerIsRaising)
          else
            // just a raise
            require(
              totalBet >= callAmount + configuration.getBigBlind,
              "Have to bet more!"
            )
            state.applyBet(
              user,
              userIndex,
              amount,
              Status.Playing,
              playerIsRaising
            )

  /** Returns state with a fold event applied.
    *
    * @param user
    *   a user id.
    * @param userIndex
    *   a user id index.
    * @return
    *   state with a fold event applied.
    */
  def applyFold(user: UserId, userIndex: Int): State =
    println("INFO : " + user + " is folding.")
    state
      .withPlayerUpdateStatus(userIndex, Status.Spectating)
      .withPlayerHasTalked(userIndex, true)
      .addLog(user + " has folded")
      .rotatePlayerTurn()

  /** Returns state with a check event applied.
    *
    * @param user
    *   a user id.
    * @param userIndex
    *   a user id index.
    * @return
    *   state with a check event applied.
    */
  def applyCheck(user: UserId, userIndex: Int): State =
    println("INFO : " + user + " is checking.")
    state
      .withPlayerHasTalked(userIndex, true)
      .addLog(user + " has called")
      .rotatePlayerTurn()

  /** Returns state with bet event applied.
    *
    * @param user
    *   a user id.
    * @param userIndex
    *   a user id index.
    * @param amount
    *   a bet amount.
    * @param newStatus
    *   a new status.
    * @param playerIsRaising
    *   is player raising or not.
    * @return
    *   state with bet event applied.
    */
  def applyBet(
      user: UserId,
      userIndex: Int,
      amount: Money,
      newStatus: Status,
      playerIsRaising: Boolean
  ): State =
    println("INFO : " + user + " is betting / calling.")
    
    val oldCheckAmount = state.getCallAmount()

    extension (state: State)
      def resetOrNotTheTalked() =
        if playerIsRaising then state.withNoPlayersTalked().addLog(user + " is raising by " + (state.gameInfo.players(userIndex).getBetAmount() - oldCheckAmount) + "$")
        else state.addLog(user + " called") 

      def addLogIsAllIn()=
        if newStatus == Status.AllIn then
          state.addLog("And " + user + " is all'd in!")
        else
          state

    val stateUpdated = state
      .withPlayerUpdateMoney(userIndex, -amount)
      .withPlayerUpdateBet(userIndex, amount)
      .withPlayerUpdatePotContribution(userIndex, amount)
      .withPlayerUpdateStatus(userIndex, newStatus)
      .resetOrNotTheTalked()
      .addLogIsAllIn()
      .withPlayerHasTalked(userIndex, true)
      .rotatePlayerTurn()

    println("DEBUG: DONE ")

    stateUpdated

  /** Returns state with all in (bet) event applied.
    *
    * @param user
    *   a user id.
    * @param userIndex
    *   a user id index.
    * @param amount
    *   an all in amount.
    * @param playerIsRaising
    *   is player raising or not.
    * @return
    *   state with all in (bet) event applied.
    */
  def applyAllIn(
      user: UserId,
      userIndex: Int,
      amount: Money,
      playerIsRaising: Boolean
  ): State =
    println("INFO : " + user + " is alling in !")
    state.applyBet(user, userIndex, amount, Status.AllIn, playerIsRaising)

  /** Returns state with all players not talked.
    *
    * @return
    *   state with all players not talked.
    */
  def withNoPlayersTalked(): State =
    println("DEBUG: WITH NO PLAYERS TALKED")
    val players = state.gameInfo.players
    val playersNoTalked = players.map(_.withHasTalked(false))
    val gameInfoUpdated = state.gameInfo.copy(players = playersNoTalked)

    state.copy(gameInfo = gameInfoUpdated)
    
  /** Returns state with all players with 0 pot contribution
   * @retun
   *  state with all players with 0 pot cntribution
   */

  def withNoBetContributionPlayers(): State=
    val players = state.gameInfo.players
    val playersNoContrib = players.map(_.withPotContribution(0))
    val gameInfoUpdated = state.gameInfo.copy(players = playersNoContrib)


    


    state.copy(gameInfo = gameInfoUpdated)

  /** Returns state with a player's pot contribution updated.
    *
    * @param userIndex
    *   a user index.
    * @param potContributionToAddOrSub
    *   a pot contribution to add to the player's pot contribution
    * @return
    *   state with a player's pot contribution updated.
    */
  def withPlayerUpdatePotContribution(userIndex: Int, potContributionToAddOrSub: Money): State =
    val players = state.gameInfo.players
    val playersWithIndex = players.zipWithIndex

    val playersUpdated = playersWithIndex.map((player, index) =>
      if index == userIndex then player.updatePotContribution(potContributionToAddOrSub) else player
    )

    val gameInfoUpdated = state.gameInfo.copy(players = playersUpdated)

    state.copy(gameInfo = gameInfoUpdated)
  

  /** Returns state with a player's status updated.
    *
    * @param userIndex
    *   a user index.
    * @param newStatus
    *   a (new)status.
    * @return
    *   state with a player's status updated.
    */
  def withPlayerUpdateStatus(userIndex: Int, newStatus: Status): State =
    val players = state.gameInfo.players
    val playersWithIndex = players.zipWithIndex

    val playersUpdated = playersWithIndex.map((player, index) =>
      if index == userIndex then player.withStatus(newStatus) else player
    )

    val gameInfoUpdated = state.gameInfo.copy(players = playersUpdated)

    state.copy(gameInfo = gameInfoUpdated)
  
  /** Returns state with a player's bet amount updated.
    *
    * @param userIndex
    *   a user index.
    * @param amount
    *   a bet amount.
    * @return
    *   state with a player's bet amount.
    */
  def withPlayerUpdateBet(userIndex: Int, amount: Money): State =
    val players = state.gameInfo.players
    val playersWithIndex = players.zipWithIndex

    val playersUpdated = playersWithIndex.map((player, index) =>
      if index == userIndex then player.updateBetAmount(amount) else player
    )

    val gameInfoUpdated = state.gameInfo.copy(players = playersUpdated)

    state.copy(gameInfo = gameInfoUpdated)

  /** Returns state with a player's balance updated.
    *
    * @param userIndex
    *   a user index.
    * @param moneyToAddOrSub
    *   an amount of money.
    * @return
    *   state with a player's balance updated.
    */
  def withPlayerUpdateMoney(userIndex: Int, moneyToAddOrSub: Money): State =
    val players = state.gameInfo.players
    val playersWithIndex = players.zipWithIndex

    val playersUpdated = playersWithIndex.map((player, index) =>
      if index == userIndex then player.updateMoney(moneyToAddOrSub) else player
    )

    val gameInfoUpdated = state.gameInfo.copy(players = playersUpdated)

    state.copy(gameInfo = gameInfoUpdated)

  /** Returns state with a player's "talking" updated.
    *
    * @param userIndex
    *   a user index.
    * @param hasTalked
    *   true or false.
    * @return
    *   state with a player's "talking" updated.
    */
  def withPlayerHasTalked(userIndex: Int, hasTalked: Boolean): State =
    def gameInfo = state.gameInfo
    state.copy(
      gameInfo = gameInfo.copy(
        players = gameInfo.players.updated(
          userIndex,
          gameInfo.players(userIndex).withHasTalked(hasTalked)
        )
      )
    )

  /** Returns the highest bet amount, aka call amount.
    *
    * @return
    *   highest bet amount/call amount.
    *
    * @throws UnsupportedOperationException
    *   if collection is empty.
    */
  def getCallAmount(): Money =
    state.gameInfo.players.map(_.getBetAmount()).max

  /** Returns a sequence of state when transitioning from a phase to another.
    *
    * @return
    *   a sequence of state.
    */
  def transitionPhase: Seq[State] =

    if state.gamePhase == EndRound ||
      state.gamePhase == EndGame
    then
      throw Exception(
        "The endGame / endRound phase was the 'main' state ; it should not be."
      )

    val players = state.gameInfo.players

    val oldPot = state.gameInfo.pot

    val allBetsTotal =
      players.foldLeft(0)((acc, player) => acc + player.getBetAmount())

    // update the pot
    val newPot = oldPot + allBetsTotal

    // resetTheBetAmount of the players
    val playersWithZeroBetAmount =
      players.map(player => player.withBetAmount(0).withHasTalked(false))

    val preTransitionnedState = state.copy(
      gameInfo = state.gameInfo.copy(
        players = playersWithZeroBetAmount,
        pot = newPot
      )
    ).addLog("Added " + allBetsTotal+"$ to the pot!")

    preTransitionnedState.gamePhase match
      case PreFlop => Seq(preTransitionnedState.goToFlop())
      case Flop    => Seq(preTransitionnedState.goToTurn())
      case Turn    => Seq(preTransitionnedState.goToRiver())
      case River   => preTransitionnedState.goToEndRound()
      case _ =>
        throw Exception(
          "the transition shouldn't be called in endRound / endGame as they are only 'ephemeral' states"
        )



  /** Returns state with the pot(s) distributed to each player.
    *
    * Note: See the documentation of distributePotsInternal for more
    * information.
    *
    * @param playingPlayers
    *   a list of playing players.
    * @return
    *   state with the pot(s) distributed.
    */
  def distributePots(playingPlayers: List[PlayerInfo]): State =
    state.copy(gameInfo = state.gameInfo.distributePotInternal())

  /** Returns a sequence of states when ending round.
    *
    * (Hard method) Will do several things:
    *   - distribute the pots
    *   - calculate the next state with transitionRound
    *
    * @return
    *   a sequence of state when ending round.
    */
  def goToEndRound(): Seq[State] =
    val playingPlayers = getAllPlayingPlayers(state.gameInfo)
    // error might be here, since a player
    // that is not playing, can still have contributed to the pot.
    // This should maybe be changed to "get players that contributed"
    // in case the winner is a subPot. WIll change later if we have the time

    // can simply reset players bet amount to zero if playing. if not playing don't reset?

    val endState = state.distributePots(playingPlayers).nextPhase()

    Seq(endState, endState.transitionRound())

  /** Returns flop state.
    *
    * @return
    *   flop state.
    */
  def goToFlop(): State =
    println("DEBUG: GOING TO FLOP")
    state.addCommunal(3).cleanupNextPhase().nextPhase()

  /** Returns turn state.
    *
    * @return
    *   turn state.
    */
  def goToTurn(): State =
    println("DEBUG: GOING TO TURN")
    state.addCommunal(4).cleanupNextPhase().nextPhase()

  /** Returns river state.
    *
    * @return
    *   river state.
    */
  def goToRiver(): State =
    println("DEBUG: GOING TO RIVER")
    state.addCommunal(5).cleanupNextPhase().nextPhase()

  /** Returns state cleaned up.
    *
    * A method that makes all the cleanup needed for the next phase: 1 - set
    * minRaise 2 - set the right order
    *
    * @return
    *   state cleaned up.
    */
  def cleanupNextPhase(): State =
    state.setMinRaise().setBeginOfRoundOrder()

  /** Returns state with new communal card(s) added.
    *
    * @param outputCommunalCardLength
    *   number of cards to display.
    * @return
    *   state with new communal card(s) added.
    */
  def addCommunal(outputCommunalCardLength: Int): State =

    val communalCards = state.gameInfo.communalCards
    val deck = state.deck

    if communalCards.length > outputCommunalCardLength then
      throw IllegalArgumentException("State has too many communal cards.")
    else
      val cardsToAdd = outputCommunalCardLength - communalCards.length
      println("INFO : adding " + cardsToAdd + " cards to the communal cards.")

      val newDeck = deck.drop(cardsToAdd)
      val newCommunalCards =
        communalCards ++ deck.take(cardsToAdd)

      state.copy(
        deck = newDeck,
        gameInfo = state.gameInfo.copy(
          communalCards = newCommunalCards
        )
      )

  /** Returns state after a round.
    *
    * @return
    *   state after a round.
    */
  def transitionRound(): State =
    if state.gameInfo.roundNumber >= state.gameConfig.maxRound || state.setStatus().gameInfo.players.count(_.isPlaying()) <= 2  then
      // game is ended
      state.endGame()
    else
      state
        .increaseRoundNumber()
        .nextPhase()
        .setStatus()
        .rotatePlayerRole()
        .withNoBetContributionPlayers()
        .withNoPlayersTalked() //this might be useless, but still we never know
        .setBeginOfRoundOrder()
        .populateShuffledDeck()
        .distributeCards()
        .resetFlop()
        .populateBlinds
        .executeBlinds()
        .setMinRaise()
        .addLog("Started new round")
        


  /** Returns state with number of rounds increased.
    *
    * @return
    *   state with number of rounds increased.
    */
  def increaseRoundNumber(): State =
    state.copy(
      gameInfo = state.gameInfo.copy(
        roundNumber = state.gameInfo.roundNumber + 1
      )
    )

  /** Returns state with status set for each player. If player is out of money,
    * he can't play anymore.
    *
    * @return
    *   state with status set for each player.
    */
  def setStatus(): State =
    state.copy(
      gameInfo = state.gameInfo.copy(
        players = state.gameInfo.players.map(player =>
          if player.getMoney() <= 0 then
            player.withMoney(0).withStatus(Status.Spectating)
          else player.withStatus(Status.Playing)
        )
      )
    )

  /** Returns state with blinds executed.
    *
    * Player is all in if 0 < balance < blind
    *
    * @return
    *   state with all blinds executed.
    */
  def executeBlinds(): State =
    val players = state.gameInfo.players
    val smallBlind = state.gameConfig.smallBlind
    val bigBlind = state.gameConfig.bigBlind

    val updatedPlayers = players.map(player =>
      player.getRole() match
        case SmallBlind =>
          if player.getMoney() < smallBlind then player.withStatus(Status.Spectating) else
            player.updateBetAmount(smallBlind).updateMoney(-smallBlind).updatePotContribution(smallBlind)

        case BigBlind => 
          if player.getMoney() < bigBlind then player.withStatus(Status.Spectating) else 
            player.updateBetAmount(bigBlind).updateMoney(-bigBlind).updatePotContribution(bigBlind)

        case _ => player
      )

    val gameInfoUpdated = state.gameInfo.copy(players = updatedPlayers)
    state.copy(gameInfo = gameInfoUpdated)

    /*
    val smallBlindPlayer =
      players.find(player => player.getRole() == SmallBlind).get
    val bigBlindPlayer =
      players.find(player => player.getRole() == BigBlind).get

    val stateSB =
      applyEventNaive(smallBlindPlayer.getUserId(), Event.Bet(smallBlind))

    val stateBB =
      stateSB.applyEventNaive(bigBlindPlayer.getUserId(), Event.Bet(bigBlind))

    stateBB*/

  /** Returns state with communal cards reset.
    *
    * @return
    *   state with communal cards reset.
    */
  def resetFlop(): State =
    state.copy(
      gameInfo = state.gameInfo.copy(
        communalCards = Nil
      )
    )

  /** Returns state with a sentence added to the log.
    *
    * @param str
    *   a sentence.
    * @return
    *   state with a sentence added to the log.
    */
  def addLog(str: String): State =
    val stateLog = state.gameInfo.logs
    val updatedLog = stateLog :+ str

    val gameInfoUpdated = state.gameInfo.copy(logs = updatedLog)

    state.copy(gameInfo = gameInfoUpdated)

  /** Returns state with the minimum raise set. (Always big blind).
    *
    * @return
    *   state with the minimum raise set.
    */
  def setMinRaise(): State =
    val bigBlind = state.gameConfig.bigBlind

    val gameInfoUpdated = state.gameInfo.copy(minRaise = bigBlind)

    state.copy(gameInfo = gameInfoUpdated)

  /** Returns true if all players have talked, else false.
    *
    * @return
    *   true if all players have talked, else false.
    */
  def hasEveryoneTalked: Boolean =
    state.gameInfo.players.filter(_.isOnlyPlaying()).forall(player => player.hasTalked())

  /** Returns true if everyone has called/bet same amount, else false.
    *
    * @return
    *   true if everyone has called, else false.
    */
  def hasEveryoneBettedSameAmount: Boolean =
    val callAmount = state.getCallAmount()
    val betAmounts = state.gameInfo.players
      .map(player => if player.isOnlyPlaying() then player.getBetAmount() else callAmount)

    println("DEBUG: BET AMOUNTS -> " + betAmounts)
    betAmounts.forall(bet => bet == callAmount)


  /** Returns state with end game phase.
    * @todo
    *   logs
    * @return
    *   state at end game.
    */
  def endGame(): State =
    state.copy(
      gamePhase = EndGame
    )


extension (gameInfo: GameInfo)

  /** Returns list of players that are still playing (all in included).
    *
    * @return
    *   list of players that are still playing.
    */
  def getAllPlayingPlayers: List[PlayerInfo] =
    gameInfo.players.filter(player => player.isPlaying())


  /** Returns list of players that are only playing.
    *
    * @return
    *   list of players that are only playing.
    */
  def getAllOnlyPlayingPlayers: List[PlayerInfo] =
    gameInfo.players.filter(player => player.isOnlyPlaying())

  /** Returns gameInfo with players turn rotated.
    *
    * @return
    *   gameInfo with players turn rotated.
    */
  def rotatePlayerTurnInternal(): GameInfo = //TODO check players that are not playing
    gameInfo.copy(players = {
      val oldPlayers = gameInfo.players
      oldPlayers.drop(1).appended(oldPlayers.head)
    })

  /** Returns gameInfo with players role rotated. Simply put, new dealers, small
    * blind, big blind assigned. Makes sure to skip players not playing.
    *
    * @return
    *   gameInfo with players role rotated
    */
  def rotatePlayerRolesInternal(): GameInfo =

    val players = gameInfo.players

    println("DEBUG: rotatePlayerRoles before players: " + players)

    require(players.size >= 3)
    val playersWithIndex = players.zipWithIndex

    val lastBigBlind = playersWithIndex.find((player, _) => player.isBigBlind())

    val newPlayers = lastBigBlind match
      case Some((bigBlindPlayer, indexBB)) =>
        val indexOfBigBlind = indexBB

        playersWithIndex.map((player, index) =>
          if player.isPlaying() then
            player.getRole() match
              case Dealer =>
                player.withRole(
                  players(Math.floorMod((index - 1), players.size)).getRole()
                )
              case SmallBlind => player.withRole(Dealer)
              case BigBlind   => player.withRole(SmallBlind)
              case Normal =>
                if index == ((indexOfBigBlind + 1) % players.length) then
                  player.withRole(BigBlind)
                else player
          else player
        )

      case None => throw Exception("No Big Blind in the game ??")

      println("DEBUG: rotatePlayerRoles after players (result): " + newPlayers)
    gameInfo.copy(players = newPlayers)

  /** Returns game info with players order of the round. For preflop player
    * after big blind starts. For flop, turn, river blind starts.
    *
    * @TODO
    *   since we execute blinds, begin of round is always small blind no?
    * @TODO
    *   players playing?
    * @return
    *   gameInfo with players order of the round.
    */
  def setBeginOfRoundOrderInternal(state: State): GameInfo =
    val players = gameInfo.players

    println("DEBUG: setBeginOfRoundOrder, players before set: " + players)

    val smallBlindPosition = players.indexWhere(player => player.isSmallBlind())
    val bigBlindPosition = players.indexWhere(player => player.isBigBlind())

    assert(smallBlindPosition >= 0, "No Small Blind?!")
    assert(bigBlindPosition >= 0, "No Big Blind?!")

    state.gamePhase match
      case PreFlop =>
        gameInfo.copy(players =
          players.drop(bigBlindPosition + 1) ++ players.take(
            bigBlindPosition + 1
          )
        )

      case Flop | Turn | River =>
        gameInfo.copy(players =
          players.drop(smallBlindPosition) ++ players.take(smallBlindPosition)
        )

      case EndRound | EndGame =>
        throw Exception(
          "no order needed for endRound/endGame"
        ) // should never happen

  /** This methods does : 1 - finds winner 2 - gives him/her the money it
    * deserves 3 - toggles it from the pot 4 - if pot not empty, recurses (case
    * of multi pot) TODO potential problem: if winner is alled in , but noe
    * layer folded, his bet will not be taken into account ; will fix after
    */
  /** Returns gameInfo with pots distributed.
    *
    * @param playingPlayers
    *   a list of playing players
    * @return
    *   gameInfo with pots distributed.
    */
    /*
  def distributePotInternal2(playingPlayers: List[PlayerInfo]): GameInfo =

    assert(
      playingPlayers.forall(_.isPlaying()),
      "the players passed to distributePot must only be playing"
    )

    val allPlayers = gameInfo.players
    val communalCards = gameInfo.communalCards

    val winner = CardHelper.findWinner(playingPlayers, communalCards).head

    val winnerIndex = allPlayers.indexWhere(_ == winner)

    assert(winnerIndex >= 0, "No winner ??")

    val pot = gameInfo.pot

    winner.getStatus() match
      case Status.Spectating =>
        throw Exception("The winning player is not playing")

      case Status.Playing =>
        val winnerWithMoney = winner.updateMoney(pot)
        gameInfo.copy(
          players = allPlayers.updated(winnerIndex, winnerWithMoney),
          pot = 0
        )

      case Status.AllIn =>
        // mediocre code, I am sorry
        // test case : a = 20 b = 10 c =30 d = 40 e = 40
        // winners : a > c > e
        //
        // a wins over b;c;d;e
        //            10; 20 ; 20 ;20
        // new state of pot contributions :
        //            0; 10; 20; 20
        // c wins over d;e
        //            10, 10
        // new state : 10, 10
        // e wins over d
        //            10
        val (updatedPlayers, amountHeWins) =
          playingPlayers.foldLeft((Nil, 0))((rest, p) =>
            if p == winner then rest
            else if p.getPotContribution() >= winner.getPotContribution() then
              (
                p.updatePotContribution(
                  -winner.getPotContribution()
                ) :: rest._1,
                rest._2 + winner.getPotContribution()
              )
            else
              (
                p.withPotContribution(0) :: rest._1,
                rest._2 + p.getPotContribution()
              )
          )

        val newPot = pot - amountHeWins

        assert(newPot >= 0, "Pot <= 0 ??")

        val winnerWithPot = winner.updateMoney(amountHeWins)

        val newGameInfo =
          gameInfo.copy(
            players = allPlayers.updated(
              winnerIndex,
              winnerWithPot
            ),
            pot = newPot
          )

        if newPot == 0 then newGameInfo
        else
          val playingPlayersWithoutLastWinner =
            playingPlayers.filter(p => p != winner)

          assert(
            playingPlayersWithoutLastWinner.length == playingPlayers.length - 1,
            "Problem with how the last winner is toggled from the list"
          )

          newGameInfo.distributePotInternal(
            playingPlayersWithoutLastWinner
          )*/


  def createSidePots(): List[(List[UserId], Money)] =
    val players = gameInfo.players

    val contributions: List[(UserId, Money)] = players.map(player => (player.getUserId(), player.getPotContribution()))

    val sortedContributions = contributions.sortBy((_, contribution) => contribution)

    def newSidePot(remainingMoney: List[(UserId, Money)]): List[(List[UserId], Money)] =
      
      val zeroFiltered = remainingMoney.filter((_, contribution) => contribution > 0)

      if zeroFiltered.isEmpty then Nil else 

        val smallestContr = zeroFiltered.head._2
        val thisPotPlayers = zeroFiltered.map((userId, _) => userId)

        val thisPotSize = smallestContr * zeroFiltered.size

        val remainingUpdated = 
          remainingMoney.map((userId, contr) => if contr > 0 then (userId, contr - smallestContr) else (userId, contr) )

        (thisPotPlayers, thisPotSize) :: newSidePot(remainingUpdated.sortBy(_._2))

    newSidePot(sortedContributions)

  def distributePotInternal(): GameInfo = 
    
    val sidePots = createSidePots()

    println("DEBUG: Side POTS " + sidePots)
    val players = gameInfo.players

    val communalCards = gameInfo.communalCards

    val winnersWithMoneyWon = sidePots.map(pot =>
      val (potPlayersId, potSize) = pot

      val playersInPot = players.filter(player => potPlayersId.contains(player.getUserId()))

      
      //find winner only considers players playing
      val winners = CardHelper.findWinner(playersInPot, communalCards)
      
      println("DEBUG: Winner is " + winners)
      (winners.map(_.getUserId()), potSize)
    
    )

    println("DEBUG: Winners with money won! " + winnersWithMoneyWon)
    
    val playersEarnings = winnersWithMoneyWon.foldLeft(Map[UserId, Money]())(
      (acc, winWithMoney) =>
        val (winnersId, potSize) = winWithMoney
        //case that there a no winners (a side pot where everyone folded)
        if winnersId.nonEmpty then 
          val moneyWon = potSize / winnersId.size
          winnersId.foldLeft(acc)((newAcc, winnerId) =>
            newAcc.updated(winnerId, newAcc.getOrElse(winnerId, 0) + moneyWon))
        else 
          acc
    )


    val playersUpdated = players.map(player =>
      val moneyWon = playersEarnings.getOrElse(player.getUserId(), 0)
      player.updateMoney(moneyWon)
    )
    gameInfo.copy(players = playersUpdated, pot = 0)









    

