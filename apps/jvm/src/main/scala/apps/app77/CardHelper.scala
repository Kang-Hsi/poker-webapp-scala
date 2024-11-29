package apps.app77

import scala.util.Random

object CardHelper:
  
  /**
   * The basic ordonned deck of cards
**/ 
  val allCards: Deck = 
    val repr = allRepresentations.toIterator
    (for 
      suit <- Suit.values
      nb <- 1 to 13
    yield
      (suit, nb, repr.next())
    ).toList                      


  extension (d:Deck)
    def shuffle()=
      Random.shuffle(d)
  
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

