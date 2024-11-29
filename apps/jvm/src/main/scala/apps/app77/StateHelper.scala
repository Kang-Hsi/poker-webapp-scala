package apps.app77

import apps.app77.CardHelper.shuffle
import apps.app77.Logic
//TODO maybe rename the class name

extension (st: State)
  def shuffleDeck()=
    st.copy(deck = st.deck.shuffle())

  /**
   * Creates a new state with each players 2 cards assgiend to him.
   * The Deck is also updated
  **/
  def assignCardsToPlayers()=
    val deckIterator = st.deck.iterator

    val gameInfo = st.gameInfo
    
    val players = gameInfo.players

    val playersWithCards = (for 
      pl <- players
    yield
      pl.withOptionHand(Some(
        Set(deckIterator.next(),
            deckIterator.next())
    ))).toList

    st.copy(
      deck = deckIterator.toList,
      gameInfo = gameInfo.copy(
        players = playersWithCards 
      )
      )
       
  def rotatePlayerTurn()=
    st.copy(gameInfo = st.gameInfo.rotatePlayerTurnInternal())

  /**
   * Rotates the role of the players; This function is implementein a "hard way",
   * Since we have to assume it could be called anytime
   *
   * So this functions rotates the players roles BUT
   * Does not rotate to the next player 
   * Sets the amount of BigBlind & SmallBlind to 0
   **/
  def rotatePlayerRole()=
    st.copy(gameInfo = st.gameInfo.rotatePlayerRolesInternal())

  /**
   * Set the player with smallBlind as first player to play etc...
  **/
  def setBeginOfRoundOrder()=
   st.copy(gameInfo = st.gameInfo.setBeginOfRoundOrderInternal())



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
            case SmallBlind(amount) => p.withRole(Dealer)
            case BigBlind(amount) => p.withRole(SmallBlind(0))
            case Normal => 
              if i == ((indexOfBigBlind + 1) % players.length) then
                p.withRole(BigBlind(0))
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
    
    

