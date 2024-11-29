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
       



