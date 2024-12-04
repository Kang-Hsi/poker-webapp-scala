package apps.app77

import scala.util.Random
import cs214.webapp.UserId


object CardHelper:

/**
   * Used Chat GPT for generating this list
  **/
  val allRepresentations = List(
  // Hearts
  "🂱", "🂲", "🂳", "🂴", "🂵", "🂶", "🂷", "🂸", "🂹", "🂺", "🂻", "🂽", "🂾",
  // Diamonds
  "🃁", "🃂", "🃃", "🃄", "🃅", "🃆", "🃇", "🃈", "🃉", "🃊", "🃋", "🃍", "🃎",
  // Spades
  "🂡", "🂢", "🂣", "🂤", "🂥", "🂦", "🂧", "🂨", "🂩", "🂪", "🂫", "🂭", "🂮",
  // Clubs
  "🃑", "🃒", "🃓", "🃔", "🃕", "🃖", "🃗", "🃘", "🃙", "🃚", "🃛", "🃝", "🃞"
)

  /**
   * The basic ordered deck of cards
**/
  val allCards: Deck =
    val repr = allRepresentations.iterator
    (for
      suit <- Suit.values
      nb <- 1 to 13
    yield
      (suit, nb, repr.next())
    ).toList


  /** Find the winner from a list of PlayerInfo
    * Not sue if the playerrs are already filtered out to be players that are playign, so keep checking
    * 
    *
    * @return
    */
  def findWinner(players: List[PlayerInfo]): PlayerInfo = 
    val allHands = players.flatMap{
      player => 
        if (player.isPlaying()) then player.getHand() 
        else None
    }

    ???


  
  

  extension (d:Deck)
    def shuffle()=
      Random.shuffle(d)



