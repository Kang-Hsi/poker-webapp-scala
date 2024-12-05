package apps.app77

import apps.app77.CardHelper.shuffle
import apps.app77.Logic
import apps.app77.GamePhase.*
import apps.app77.Role.*
import cs214.webapp.*

extension (state: State)

  /**
    * Returns state with new deck shuffled.
    *
    * @return state with new deck shuffled.
    */
  def populateShuffledDeck =
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
        pl <- players
        if (pl.isPlaying())
      } yield pl.withOptionHand(
        Some(
          Set(deckIterator.next(), deckIterator.next())
        )
      )).toList

    state.copy(
      deck = deckIterator.toList,
      gameInfo = gameInfo.copy(
        players = playersWithCards
      )
    )

  /**
    * Returns state with players turn rotated.
    *
    * @return state with players turn rotated.
    */
  def rotatePlayerTurn(): State =
    state.copy(gameInfo = state.gameInfo.rotatePlayerTurnInternal())

  /** Rotates the role of the players; This function is implemented in a "hard
    * way", Since we have to assume it could be called anytime.
    *
    * So this functions rotates the players roles BUT does not rotate to the
    * next player. Sets the amount of BigBlind & SmallBlind to 0.
    */
  def rotatePlayerRole() =
    state.copy(gameInfo = state.gameInfo.rotatePlayerRolesInternal())

  /** Set the player with smallBlind as first player to play etc...
    * Except on preflop.!!!
    * @TODO
    *   skips spectacting
    */
  def setBeginOfRoundOrder() =
    state.copy(gameInfo = state.gameInfo.setBeginOfRoundOrderInternal(state))

  /** To be called before the start of a round. Will populate the state with a
    * new deck. Will rotate the players roles & set the right order. Will populate
    * the players with cards.
    *
    * USELESS NOW
    */
  def startRound() =

    import apps.app77.CardHelper.shuffle
    val stateWithNewRoles = state.rotatePlayerRole()
    val stateWithRightOrder =
      stateWithNewRoles.setBeginOfRoundOrder()

    val stateWithNewShuffledDeck =
      stateWithRightOrder.copy(
        deck = CardHelper.allCards.shuffle()
      )

   // stateWithNewShuffledDeck.distributeCardsToPlayers().populateBlinds

   
  /** fais tout le nécéssaire pour finir le round (trouves le winner, lui donnes
    * largent) applée juste avant startRound
    *
    * Will find winner and add to his money the pot amount Will set pot to 0 ?
    * NEST PLUS UTILE / VALABLE 
    */
  def endRound(): State =
    val State(gamePhase, gameInfo, deck, gameConfig) = state
    require(gamePhase == GamePhase.EndRound)

    val winner = CardHelper.findWinner(state.gameInfo.players, state.gameInfo.communalCards);

    val players = gameInfo.players;

    // plus simple de faire une map? obligé bah de parcourir tt les joueurs
    val playersUpdated = players.map(player =>
      if player.getUserId() == winner.head.getUserId() then player.updateMoney(gameInfo.pot)
      else player
    )

    // val updatedGameInfo = gameInfo.copy(pot = 0)
    val gameInfoUpdated = gameInfo.copy(players = playersUpdated)
    val newState = state.copy(gameInfo = gameInfoUpdated)
    newState.nextPhase() 

  /** Populate the blind amount in functio of config
    */
  def populateBlinds(using Configuration): State =
    val gameConfigWithBlinds = state.gameConfig
      .copy(smallBlind = conf.getSmallBlind, bigBlind = conf.getBigBlind)

    state.copy(gameConfig = gameConfigWithBlinds)


  /** Returns state with next phase (if possible)
    *
    * @return
    *   state with next phase, or just itself.
    */
  def nextPhase(): State =
      state.gamePhase match
        case PreFlop  => state.copy(gamePhase = Flop)
        case Flop     => state.copy(gamePhase = Turn)
        case Turn     => state.copy(gamePhase = River)
        case River    => state.copy(gamePhase = EndRound)
        case EndRound => state.copy(gamePhase = PreFlop)
        case EndGame  => throw Exception("Game has ended !, No nextPhase")

  /** Returns the highest bet amount.
    *
    * @return
    *   the highest bet amount
    */
  def highestBetAmount(): Money =
    val players = state.gameInfo.players

    val maxAmount = players.map(_._6).maxOption

    maxAmount match
      case None      => throw Exception("Empty list")
      case Some(max) => max

  /** Applies (naively) event updating only the money, the pot and status.
    *
    * @TODO
    *   check if event is legal
    *
    * @param e
    * @return
    */
  def applyEventNaive(user:UserId,event: Event):State = 
    val players = state.gameInfo.players   
    
    val userIndex = players.indexWhere(_.getUserId() == user)

    if userIndex < 0 then throw IllegalMoveException("unknown user")

    if !players(userIndex).isOnlyPlaying() then
      throw IllegalMoveException("You are spectating / Alled in, you cannot do any action")

    //the user is ensured to be playing
    
    //the player must then be its turn:

    if userIndex != 0 then throw IllegalMoveException("please wait your turn")

    event match
      case Event.Fold() => 
        //in which cases a playyer cannot fold ?
        //if he is the last player "playing" ?
        //i guess

        val allPlaying = state.gameInfo.getAllPlayingPlayers
        if allPlaying.length == 1 then throw IllegalMoveException("You cannot fold if you are the last one playing / all'd in")

        state.applyFold(user,userIndex)
      case Event.Check() => 
        //in which cases a player cannot check ?
        //1 - if he has not got the correct bet amount
        // TODO what else?
        if players(userIndex).getBetAmount() < state.getCallAmount()
        then throw IllegalMoveException("You cannot check, as you need to bet more money to call")
        
        state.applyCheck(user, userIndex)

      case Event.Bet(amount) =>
        //in which cases we cannot bet ?
        // 1 - if the amount we bet is outside how much money we have
        // TODO what else?
        //
        assert(amount > 0, IllegalMoveException("cannot bet <= 0"))
        
        if players(userIndex).getMoney() < amount then throw IllegalMoveException("Not enough money")

        if players(userIndex).getBetAmount() + amount < state.getCallAmount()
        then
          if players(userIndex).getMoney() == amount then
            //we are in all in 
            state.applyAllIn(user, userIndex, amount, false)
          else
            throw IllegalMoveException("Not enough money to call, and not an all in neither")

        else
          val overBetting = players(userIndex).getBetAmount() + amount > state.getCallAmount()
          if players(userIndex).getMoney() == amount then
            state.applyAllIn(user,userIndex, amount, overBetting)
          else
            //we should be good
            state.applyBet(user, userIndex,amount, Status.Playing, overBetting )
          


  /**
   * Applies the event given. Does not check if the event is valid, the check must be done before
  **/
  def applyFold(user:UserId, index:Int):State=
    println("INFO : " + user + " is folding.")
    state
      .withPlayerUpdateStatus(index, Status.Spectating)
      .withPlayerHasTalked(index, true)
      .rotatePlayerTurn()


  def applyCheck(user:UserId, index: Int)=
    println("INFO : " + user + " is checking.")
    state.withPlayerHasTalked(index, true).rotatePlayerTurn()


  def applyBet(user:UserId, index:Int, amount:Money, newStatus: Status, overBetting: Boolean)=
    println("INFO : " + user + " is betting / calling.")
    
    extension (s:State)
      def resetOrNotTheTalked()=
        if overBetting then
          s.withNoPlayersTalked()
        else
          s

    state
      .withPlayerUpdateMoney(index, -amount)
      .withPlayerUpdateBet(index, amount)
      .withPlayerUpdateStatus(index, newStatus)
      .resetOrNotTheTalked()
      .withPlayerHasTalked(index, true)
      .rotatePlayerTurn()


  def applyAllIn(user:UserId, index: Int, amount: Money, overBetting: Boolean)=
    println("INFO : " + user + " is alling in !")
    state.applyBet(user,index,amount,Status.AllIn, overBetting) 
  /**
   * Sets all the players to no talk
  **/
  def withNoPlayersTalked(): State =

    val players = state.gameInfo.players
    val playersNoTalked = players.map(_.withHasTalked(false))
    val gameInfoUpdated = state.gameInfo.copy(players = playersNoTalked)

    state.copy(gameInfo = gameInfoUpdated)


  def withPlayerUpdateStatus(userIndex: Int, newStatus: Status): State=
    val players = state.gameInfo.players
    val playersWithIndex = players.zipWithIndex

    val playersUpdated = playersWithIndex.map((player, index) => if index == userIndex then player.withStatus(newStatus) else player)

    val gameInfoUpdated = state.gameInfo.copy(players = playersUpdated)

    state.copy(gameInfo = gameInfoUpdated)


  def withPlayerUpdateBet(userIndex:Int, amount:Money):State=
    val players = state.gameInfo.players
    val playersWithIndex = players.zipWithIndex

    val playersUpdated = playersWithIndex.map((player, index) => if index == userIndex then player.withBetAmount(amount) else player)

    val gameInfoUpdated = state.gameInfo.copy(players = playersUpdated)

    state.copy(gameInfo = gameInfoUpdated)

  def withPlayerUpdateMoney(userIndex: Int, moneyToAddOrSub: Money):State=
    val players = state.gameInfo.players
    val playersWithIndex = players.zipWithIndex

    val playersUpdated = playersWithIndex.map((player, index) => if index == userIndex then player.withMoney(moneyToAddOrSub) else player)

    val gameInfoUpdated = state.gameInfo.copy(players = playersUpdated)

    state.copy(gameInfo = gameInfoUpdated)


  def withPlayerHasTalked(userIndex:Int, hasTalked : Boolean):State=
    def gameInfo = state.gameInfo
    state.copy(
      gameInfo = gameInfo.copy(
        players = gameInfo.players.updated(
          userIndex,
          gameInfo.players(userIndex).withHasTalked(hasTalked)
          )
        )
      )

  /**
   * Gets the call amount
   * could be done using the callAmount in gameInfo, but unsure if we will keep it so..
  **/
  def getCallAmount():Money=
    state.gameInfo.players.map(_.getBetAmount()).max


  /** Transitions from a phase to another
    *
    * @return
    */
  def transitionPhase: Seq[State] = 
    
    if state.gamePhase == EndRound || 
        state.gamePhase == EndGame then
          throw Exception("The endGame / endRound phase was the 'main' state ; it should not be.")
      

    val players = state.gameInfo.players
    
    val oldPot = state.gameInfo.pot 

    val betsOfPlayers = players.foldLeft(0)((r,p) => r+p.getBetAmount())
    // update the pot
    val newPot = oldPot + betsOfPlayers
    // resetTheBetAmount of the players
    
    val playersWithZeroBetAmount = players.map(p => p.withBetAmount(0).withHasTalked(false)) //here we reset the talked like this, bu we could have called the method resettalked
    
    val preTransitionnedState = state.copy(
      gameInfo = state.gameInfo.copy(
        players = playersWithZeroBetAmount,
        pot = newPot
      )
    )

    preTransitionnedState.gamePhase match
      case PreFlop => Seq(preTransitionnedState.goToFlop())
      case Flop => Seq(preTransitionnedState.goToTurn())
      case Turn => Seq(preTransitionnedState.goToRiver())
      case River => preTransitionnedState.goToEndRound()
      case _ => throw Exception("the transition shouldn't be called in endRound / endGame as they are only 'ephemeral' states")      

  /** Distributes the pots to each player based on the algorithm.
   *  See the documentation of distributePotsInternal for more accurate info
    *
    * @param playingPlayers
    * @return
    */
  def distributePots(playingPlayers: List[PlayerInfo]): State = 
    state.copy(gameInfo = state.gameInfo.distributePotInternal(playingPlayers))    


  /** Hard method:
   *  Will do several things:
   *  distribute the pots
   *  and calculate the nex state with transitionRound
   *
  **/
  def goToEndRound(): Seq[State]=
    val playingPlayers = getAllPlayingPlayers(state.gameInfo) //error might be here, since a player
  //that is not playing, can still have contributed to the pot.
  //This should maybe be changed to "get players that contributed" 
  //in case the winner is a subPot. WIll change later if we have the time
    val endState = state.distributePots(playingPlayers).nextPhase()

    Seq(endState, endState.transitionRound()) 
  
  /**
   * Method that handles the transitition to go to flop
  **/
  def goToFlop()=
    state.addCommunal(3).cleanupNextPhase().nextPhase()

  def goToTurn()=
    state.addCommunal(4).cleanupNextPhase().nextPhase()

  def goToRiver()=
    state.addCommunal(5).cleanupNextPhase().nextPhase()


  /**
   * A method that makes all the cleanup needed for the next phase:
   * 1 - right minRaise setted
   * 2 - Sets the right order
  **/
  def cleanupNextPhase()=
    state.setMinRaise().setBeginOfRoundOrder()
    

  /**
   * Method that adds cards to the communal cards
   * util the number of communal cards gets to the
   * input
  **/
  def addCommunal(outputCommunalCardLength: Int):State=
    val communal = state.gameInfo.communalCards 
    if communal.length > outputCommunalCardLength then
      throw Exception("the state has too many communal cards, there is a discordancy beetween the gamePhase, and its real phase")
    else
      val cardsToAdd = outputCommunalCardLength - communal.length
      println("INFO : adding " + cardsToAdd + " cards to the communal cards.")
      
      val newDeck = state.deck.drop(cardsToAdd)
      val newCommunal =
        state.gameInfo.communalCards ++ state.deck.take(cardsToAdd)

      state.copy(
        deck = newDeck,
        gameInfo = state.gameInfo.copy(
          communalCards = newCommunal
        )
      )


  //method that transitions round
  def transitionRound(): State=
    if state.gameInfo.roundNumber >= state.gameConfig.maxRound then
      //game is ended
      state.endGame()
    else
      state
        .increaseRoundNb()
        .setStatus()
        .rotatePlayerRole()
        .setBeginOfRoundOrder()
        .populateShuffledDeck
        .distributeCards()
        .resetFlop()
        .populateBlinds
        .executeBlinds()


  def increaseRoundNb():State=
    state.copy(
      gameInfo = state.gameInfo.copy(
        roundNumber= state.gameInfo.roundNumber + 1
      )
    )

  /**
   * Sets the default status of the players at the beginning of a round (BEFORE that the smallblinds/ bigBlinds are payed)
   * This method will set the player to spectating if he has no more money. Should only be called before any bidding happens (and before the smallBlind / bigBlind get executed)
  **/
  def setStatus()=
    state.copy(
      gameInfo = state.gameInfo.copy(
        players = state.gameInfo.players.map(
          p => if p.getMoney() <= 0 then
            p.withMoney(0).withStatus(Status.Spectating)
          else
            p
        )
      )
    )


  /**
   * Executes the blinds,
   * updates the player state accordingly
   * If not enough money, what should we do? 
   * Id say just make him loose (spectating + allmoney to pot)
  **/
  def executeBlinds()=
    ???
  
  /**
   * Resets the communal cards
  **/
  def resetFlop():State=
    state.copy(
      gameInfo = state.gameInfo.copy(
        communalCards = Nil
      )
    )

  /** Adds sentence to the log.
    *
    * @return
    */
  def addLog(str: String): State = 
    val stateLog = state.gameInfo.logs
    val updatedLog = stateLog :+ str 

    val gameInfoUpdated = state.gameInfo.copy(logs = updatedLog)

    state.copy(gameInfo = gameInfoUpdated)

    

  /** Set the minimum raise at the beginning of the round. Always small blind.
    * @TODO should we get smallBlind from gameConfig??? 
    * @return
    */
  def setMinRaise(): State =
    val smallBlind = conf.getSmallBlind

    val gameInfoUpdated = state.gameInfo.copy(minRaise = smallBlind) 

    state.copy(gameInfo = gameInfoUpdated)

  def hasEveryoneTalked:Boolean= 
    state.gameInfo.players.forall(p =>
        p.hasTalked()
        )

  def hasEveryoneBettedSameAmount:Boolean = 
    val betAmounts = state.gameInfo.players.map(_.getBetAmount())
    betAmounts.forall(a => a == betAmounts.head)


  /**
   * Sets the phase to endGame
   * Logs the winner of the game
   * TODO needs to be done
  **/
  def endGame(): State=
    state.copy(
      gamePhase = EndGame
    )


extension (gameInfo: GameInfo)

  def getAllPlayingPlayers: List[PlayerInfo]=
    gameInfo.players.filter(
      p => p.isPlaying()
      )

  /**
    * Returns gameInfo with players turn rotated.
    *
    * @return gameInfo with players turn rotated.
    */
  def rotatePlayerTurnInternal(): GameInfo =
    gameInfo.copy(players = {
      val oldPlayers = gameInfo.players
      oldPlayers.drop(1).appended(oldPlayers.head)
    })

  /**
    * Returns gameInfo with players role rotated.
    * Simply put, new dealers, small blind, big blind assigned.
    * Makes sure to skip players not playing.
    * 
    * @return gameInfo with players role rotated
    */
  def rotatePlayerRolesInternal(): GameInfo =

    val players = gameInfo.players

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
                player.withRole(players(Math.floorMod((index - 1), players.size)).getRole())
              case SmallBlind => player.withRole(Dealer)
              case BigBlind   => player.withRole(SmallBlind)
              case Normal =>
                if index == ((indexOfBigBlind + 1) % players.length) then
                  player.withRole(BigBlind)
                else player
            else player
          )

      case None => throw Exception("No Big Blind in the game ??")

    gameInfo.copy(players = newPlayers)

  /**
    * Returns game info with players order of the round.
    * For preflop player after big blind starts.
    * For flop, turn, river blind starts.
    *
    * @return gameInfo with players order of the round.
    */
  def setBeginOfRoundOrderInternal(state: State): GameInfo =
    val players = gameInfo.players

    val smallBlindPosition = players.indexWhere(player => player.isSmallBlind())
    val bigBlindPosition = players.indexWhere(player => player.isBigBlind())

    assert(smallBlindPosition >= 0, "No Small Blind?!")
    assert(bigBlindPosition >= 0, "No Big Blind?!")

    state.gamePhase match
      case PreFlop =>
        gameInfo.copy(players = 
          players.drop(bigBlindPosition + 1) ++ players.take(bigBlindPosition + 1))

      case Flop | Turn | River =>
        
        gameInfo.copy(players =
          players.drop(smallBlindPosition) ++ players.take(smallBlindPosition)
        )
      
      case EndRound | EndGame => throw Exception("no order needed for endRound/endGame")//should never happen


  /**
   * This methods does : 
   *  1 - finds winner
   *  2 - gives him/her the money it deserves
   *  3 - toggles it from the pot
   *  4 - if pot not empty, recurses (case of multi pot)
   * TODO potential problem: if winner is alled in , but noe layer folded, his bet will not be taken into account ; will fix after
  **/
  def distributePotInternal(playingPlayers: List[PlayerInfo]): GameInfo=
    
    assert(playingPlayers.forall(_.isPlaying()), "the players passed to distributePot must only be playing")

    val allPlayers = gameInfo.players
    val communalCards = gameInfo.communalCards

    val winner = CardHelper.findWinner(playingPlayers, communalCards).head

    val winnerIndex = allPlayers.indexWhere(_ == winner)

    assert(winnerIndex >= 0, "No winner ??")

    val pot = gameInfo.pot

    winner.getStatus() match
      case Status.Spectating => throw Exception("The winning player is not playing")

      case Status.Playing => 
        val winnerWithMoney = winner.updateMoney(pot)
        gameInfo.copy(
          players = allPlayers.updated(winnerIndex, winnerWithMoney),
          pot = 0
        )
     
      case Status.AllIn =>
        //mediocre code, I am sorry 
        // test case : a = 20 b = 10 c =30 d = 40 e = 40
        // winners : a > c > e
        // 
        // a wins over b;c;d;e
        //            10; 20 ; 20 ;20
        //new state of pot contributions :
        //            0; 10; 20; 20
        //c wins over d;e
        //            10, 10
        //new state : 10, 10
        //e wins over d
        //            10
        val (updatedPlayers,amountHeWins) = 
          playingPlayers.foldLeft((Nil, 0))(
            (rest,p) => if p == winner then rest
                        else 
                          if p.getPotContribution() >= winner.getPotContribution() then
                            (p.updatePotContribution(-winner.getPotContribution()) :: rest._1,
                              rest._2 + winner.getPotContribution())
                          else
                            (p.withPotContribution(0) :: rest._1, rest._2 + p.getPotContribution()) 
              )

                      
        val newPot = pot - amountHeWins

        assert(newPot >= 0, "Pot <= 0 ??")

        val winnerWithPot = winner.updateMoney(amountHeWins)

        val newGameInfo = 
          gameInfo.copy(
            players = allPlayers.updated(
              winnerIndex, winnerWithPot
            ),
          pot = newPot
          )
        
        if newPot == 0 then newGameInfo
        else
          val playingPlayersWithoutLastWinner = 
            playingPlayers.filter( p => p != winner)

          assert(playingPlayersWithoutLastWinner.length == playingPlayers.length - 1, "Problem with how the last winner is toggled from the list")

          newGameInfo.distributePotInternal(
            playingPlayersWithoutLastWinner
            )




