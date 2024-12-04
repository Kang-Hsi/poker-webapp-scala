package apps.app77

import apps.app77.CardHelper.shuffle
import apps.app77.Logic
import apps.app77.GamePhase.*
import apps.app77.Role.*
import cs214.webapp.*

extension (state: State)

  /**
    * Returns state with deck shuffled.
    *
    * @return state with deck shuffled.
    */
  def shuffleDeck =
    state.copy(deck = state.deck.shuffle())

  /** Returns a state with cards distributed to each player. Makes sure to only
    * distribute cards to players that are playing.
    *
    * @return
    *   state with cards distributed.
    */
  def distributeCardsToPlayers(): State =
    val deckIterator = state.deck.iterator
    val gameInfo = state.gameInfo
    val players = gameInfo.players

    val playersWithCards =
      (for {
        pl <- players
        if (pl.getStatus() == Status.Playing)
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
    * way", Since we have to assume it could be called anytime
    *
    * So this functions rotates the players roles BUT Does not rotate to the
    * next player Sets the amount of BigBlind & SmallBlind to 0
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

  /** To be called before the start of a round Will populize the state with a
    * new deck Will rotate the players roles & set the right order Will populate
    * the players with cards
    *
    * Will reset pot amount ? Will roundNumber-- ? Will set callAmount,
    * minRaise, maxRaise?
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

    stateWithNewShuffledDeck.distributeCardsToPlayers().populateBlinds

    
  /** fais tout le nécéssaire pour finir le round (trouves le winner, lui donnes
    * largent) applée juste avant startRound
    *
    * Will find winner and add to his money the pot amount Will set pot to 0 ?
    * NEST PLUS UTILE / VALABLE 
    */
  def endRound(): State =
    val State(gamePhase, gameInfo, deck, gameConfig) = state
    require(gamePhase == GamePhase.EndRound)

    val winner = CardHelper.findWinner(state.gameInfo.players);

    val players = gameInfo.players;

    // plus simple de faire une map? obligé bah de parcourir tt les joueurs
    val playersUpdated = players.map(player =>
      if player.getUserId() == winner.getUserId() then player.updateMoney(gameInfo.pot)
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

  /** Vérifie que l'on puisses aller a la phase d'après (oui si on est au
    * dernier joueur et tout le monde a le meme bet amount (qui joue)
    */
  def canGoNextPhase(): Boolean =

    val gamePhase = state.gamePhase
    val players = state.gameInfo.players
    val maxBetAmount = state.highestBetAmount()

    gamePhase match
      case PreFlop =>
        players.forall(player => player.hasTalked()) &&
        players.forall(_._6 == maxBetAmount)
      case Flop | Turn | River =>
        players.forall(_._6 == maxBetAmount)
      case EndRound => true
      case EndGame  => false

  /** Returns state with next phase (if possible)
    *
    * @return
    *   state with next phase, or just itself.
    */
  def nextPhase(): State =
    if canGoNextPhase() then
      state.gamePhase match
        case PreFlop  => state.copy(gamePhase = Flop)
        case Flop     => state.copy(gamePhase = Turn)
        case Turn     => state.copy(gamePhase = River)
        case River    => state.copy(gamePhase = EndRound)
        case EndRound => state.copy(gamePhase = PreFlop)
        case EndGame  => throw Exception("Game has ended !")
    else if state.gamePhase == EndRound then state.copy(gamePhase = EndGame)
    else state

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
  def applyEventNaive(user:UserId,e: Event):State = ???

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
    
    val playersWithZeroBetAmount = players.map(p => p.withBetAmount(0))
    
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


  def goToEndRound(): Seq[State]=
    ???
  
  /**
   * Method that handles the transitition to go to flop
  **/
  def goToFlop()=
    state.addCommunal(3).cleanupNextPhase()

  def goToTurn()=
    state.addCommunal(4).cleanupNextPhase()

  def goToRiver()=
    state.addCommunal(5).cleanupNextPhase()


  /**
   * A method that makes all the cleanup needed for the next phase:
   * 1 - right minRaise setted
   * 2 - Sets the right order
  **/
  def cleanupNextPhase()=
    state.setMinRaise.setBeginOfRoundOrder()
    

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
  
  /** Adds sentence to the log.
    *
    * @return
    */
  def addLog(str: String): List[String] = ???

  /** Set the minimum raise at the beginning of the round. Always small blind.
    *
    * @return
    */
  def setMinRaise: State = ???

  def hasEveryoneTalked:Boolean= 
    state.gameInfo.players.forall(p =>
        p.hasTalked()
        )

  def hasEveryoneBettedSameAmount:Boolean = 
    val betAmounts = state.gameInfo.players.map(_.getBetAmount())
    betAmounts.forall(a => a == betAmounts.head)

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
      
      case EndRound | EndGame => throw Exception("no order needed ofr endRound/ endGame")//should never happen


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

    val winner = CardHelper.findWinner(playingPlayers)

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




