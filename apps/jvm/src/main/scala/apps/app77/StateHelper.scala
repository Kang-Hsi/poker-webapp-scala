package apps.app77

import apps.app77.CardHelper.shuffle
import apps.app77.Logic
import cs214.webapp.*
//TODO maybe rename the class name

extension (state: State)

  def shuffleDeck =
    state.copy(deck = state.deck.shuffle())

  /**
   * Creates a new state with each players 2 cards assigned to him.
   * The Deck is also updated
   * 
   * @TODO skip spectacting
  **/
  def assignCardsToPlayers()=
    val deckIterator = state.deck.iterator

    val gameInfo = state.gameInfo

    val players = gameInfo.players

    val playersWithCards = (for
      pl <- players
    yield
      pl.withOptionHand(Some(
        Set(deckIterator.next(),
            deckIterator.next())
    ))).toList

    state.copy(
      deck = deckIterator.toList,
      gameInfo = gameInfo.copy(
        players = playersWithCards
      )
      )

  def rotatePlayerTurn()=
    state.copy(gameInfo = state.gameInfo.rotatePlayerTurnInternal())

  /**
   * Rotates the role of the players; This function is implemented in a "hard way",
   * Since we have to assume it could be called anytime
   *
   * So this functions rotates the players roles BUT
   * Does not rotate to the next player
   * Sets the amount of BigBlind & SmallBlind to 0 
   * @TODO make sure to skip spectacting players
   **/
  def rotatePlayerRole()=
    state.copy(gameInfo = state.gameInfo.rotatePlayerRolesInternal())

  /**
   * Set the player with smallBlind as first player to play etc...
   * 
   * @TODO skips spectacting
  **/
  def setBeginOfRoundOrder()=
   state.copy(gameInfo = state.gameInfo.setBeginOfRoundOrderInternal())

  /**
   * To be called before the start of a round
   * Will populize the state with a new deck
   * Will rotate the players roles & set the right order
   * Will populate the players with cards
   * 
   * 
   * Will reset pot amount ?
   * Will roundNumber-- ?
   * Will set callAmount, minRaise, maxRaise?
  **/
  def startRound()=

    import apps.app77.CardHelper.shuffle
    val stateWithNewRoles = state.rotatePlayerRole()
    val stateWithRightOrder =
      stateWithNewRoles.setBeginOfRoundOrder()

    val stateWithNewShuffledDeck =
      stateWithRightOrder.copy(
        deck = CardHelper.allCards.shuffle()
      )

    stateWithNewShuffledDeck.assignCardsToPlayers().populateBlinds


  

  /**
   * fais tout le nécéssaire pour finir le round
   * (trouves le winner, lui donnes largent)
   * applée juste avant startRound
   * 
   * Will find winner and add to his money the pot amount
   * Will set pot to 0 ?
  **/
  def endRound():State=
    val State(gamePhase, gameInfo, deck, gameConfig) = state
    require(gamePhase == GamePhase.EndRound)
    
    val winner = state.findWinner;

    val players = gameInfo.players;
   
    //plus simple de faire une map? obligé bah de parcourir tt les joueurs
    val playersUpdated = players.map(player =>
      if player.getUserId() == winner then player.updateMoney(gameInfo.pot)
        else player
      )

    //val updatedGameInfo = gameInfo.copy(pot = 0) 
    val gameInfoUpdated = gameInfo.copy(players = playersUpdated)
    val newState = state.copy(gameInfo = gameInfoUpdated)
    newState.nextPhase() 
  /**
   * Populate the blind amount in functio of config
  **/
  def populateBlinds(using Configuration):State=
    ???

  /**
   * Vérifie que l'on puisses aller a la phase d'après
   * (oui si on est au dernier joueur et tout le monde a le meme
   * bet amount (qui joue)
  **/
  def canGoNextPhase():Boolean=
    ???

  /**
   * Va a la nextPhase, pas vrm besoin nn?
  **/
  def nextPhase():State=
    import GamePhase.*
    //
    state.gamePhase match
      case PreFlop => state.copy(gamePhase = Flop)
      case Flop => state.copy(gamePhase = Turn)
      case Turn => state.copy(gamePhase = River)
      case River => state.copy(gamePhase = EndRound)
      case EndRound => 
        if canGoNextPhase() then state.copy(gamePhase = PreFlop) 
        else state.copy(gamePhase = EndGame)
      case EndGame => state
    
    

  /**
    * Applies (naively) event updating only the money, the pot and status.
    * 
    * @TODO check if event is legal
    *
    * @param e
    * @return
    */
  def applyEventNaive(e: Event) = ???


  /**
    * Transitions from a phase to another
    *
    * @return
    */
  def transitionPhase: State = ???

  /**
    * Distributes the pots to each player based on the algorithm.
    *
    * @param playingPlayers
    * @return
    */
  def distributePots(playingPlayers: List[PlayerInfo]): State = ???

  /**
    * Find the winner of a given state.
    *
    * @return
    */
  def findWinner: UserId = ???

  
  /**
    * Adds sentence to the log.
    *
    * @return
    */
  def addLog(str: String): List[String] = ???

  /**
    * Set the minimum raise at the beginning of the round.
    * Always small blind.
    *
    * @return
    */
  def setMinRaise: State = ???

extension( gi:GameInfo)
  def rotatePlayerTurnInternal()=
    gi.copy(players= {
      val oldPlayers = gi.players
      oldPlayers.drop(1).appended(oldPlayers.head)
    })

  
  def rotatePlayerRolesInternal()=
    val players = gi.players
    val zippedPlayers = players.zipWithIndex

    val lastBigBlind=  zippedPlayers.find( p => p._1.isBigBlind())

    val newPlayers = lastBigBlind match
      case Some(bigBlind) =>
        val indexOfBigBlind = bigBlind._2 
        import apps.app77.Role.*
        zippedPlayers.map((p,i) =>
          p.getRole() match
            case Dealer => p.withRole(players((i-1) % players.length).getRole())
            case SmallBlind => p.withRole(Dealer)
            case BigBlind => p.withRole(SmallBlind)
            case Normal =>
              if i == ((indexOfBigBlind + 1) % players.length) then
                p.withRole(BigBlind)
              else
                p

          )

      case None => throw Exception("No Big Blind in the game ??")

      gi.copy(players = newPlayers)


  def setBeginOfRoundOrderInternal()=
    val player = gi.players
    val smallBlindIndex = player.indexWhere( p => p.isSmallBlind())
    assert(smallBlindIndex >= 0, "No Small Blind Player ??")
    gi.copy(players = player.drop(smallBlindIndex) ++ player.take(smallBlindIndex))



