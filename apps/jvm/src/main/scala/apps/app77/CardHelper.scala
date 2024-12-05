package apps.app77

import scala.util.Random
import cs214.webapp.UserId
import apps.app77.HandRank


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
      nb <- 2 to 14
    yield
      (suit, nb, repr.next())
    ).toList


  /** Find the winner from a list of PlayerInfo
    * Not sue if the playerrs are already filtered out to be players that are playign, so keep checking
    * 
    *
    * @return
    */
  def findWinner(players: List[PlayerInfo], communalCards: List[Card]): List[PlayerInfo] = 
    val playersPlaying = players.filter(_.isPlaying())

    val allHands: List[(UserId, PlayerHand)] = playersPlaying.flatMap { player =>
      player.getHand().map(hand => (player.getUserId(), hand))
    }

    val allHandsWithCommunalCards = allHands.map((userId, hand) => (userId, (hand ++ communalCards)))

    val rankings: List[(UserId, HandRank)] = allHandsWithCommunalCards.map((userId, cards) =>
      (userId, HandRank.evaluateHand(cards.toList)) 
    )

    val highestRank = rankings.map((_, handRank) => handRank).max


    val winnerUserIds = rankings.filter((_, handRank) => handRank == highestRank).map((userId, handRank) => userId)

    val winners = playersPlaying.filter(player => winnerUserIds.contains(player.getUserId()))

    winners
    

  
  


  extension (d:Deck)
    def shuffle()=
      Random.shuffle(d)



