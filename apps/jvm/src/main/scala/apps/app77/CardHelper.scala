package apps.app77

object CardHelper:
  
  /**
   * The basic ordonned deck of cards
**/ 
  val allCards = 
    val repr = allRepresentations.toIterator
    (for 
      suit <- Suit.values
      nb <- 1 to 13
    yield
      (suit, nb, repr.next())
    ).toList                      
  

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

