package apps.app77

import scala.util.Random
import cs214.webapp.UserId
import apps.app77.HandRank


object CardHelper:

  /**
   * Used Chat GPT for generating list of card representations.
   */
  val allRepresentations = List(
  // Hearts
  "🂲", "🂳", "🂴", "🂵", "🂶", "🂷", "🂸", "🂹", "🂺", "🂻", "🂽", "🂾", "🂱",
  // Diamonds
  "🃂", "🃃", "🃄", "🃅", "🃆", "🃇", "🃈", "🃉", "🃊", "🃋", "🃍", "🃎","🃁",
  // Spades
  "🂢", "🂣", "🂤", "🂥", "🂦", "🂧", "🂨", "🂩", "🂪", "🂫", "🂭", "🂮","🂡",
  // Clubs
  "🃒", "🃓", "🃔", "🃕", "🃖", "🃗", "🃘", "🃙", "🃚", "🃛", "🃝", "🃞", "🃑"
)

  /**
   * The basic ordered deck of cards. 
   * From 2 to Ace (14).
   */
  val allCards: Deck =
    val representation = allRepresentations.iterator
    (for
      suit <- Suit.values
      number <- 2 to 14
    yield
      (suit, number, representation.next())
    ).toList


  /** Find the winner(s) of a poker round from a list of PlayerInfo.
    *
    * @return the winner(s) of a poker round. 
    */
  def findWinner(players: List[PlayerInfo], communalCards: List[Card]): List[PlayerInfo] = 
    val playersPlaying = players.filter(_.isPlaying())

    if playersPlaying.length == 1 then
      playersPlaying
    else 

      val allHands: List[(UserId, PlayerHand)] = playersPlaying.flatMap { player =>
        player.getHand().map(hand => (player.getUserId(), hand))
      }

      val allHandsWithCommunalCards = allHands.map((userId, hand) => (userId, (hand ++ communalCards)))

      val rankings: List[(UserId, HandRank)] = allHandsWithCommunalCards.map((userId, cards) =>
        (userId, HandRank.evaluateHand(cards.toList)) 
      )

      println("DEBUG: RANKINGS " + rankings)
      val highestRank = rankings.map((_, handRank) => handRank).max

  
      val winnerUserIds = rankings.filter((_, handRank) => handRank == highestRank).map((userId, handRank) => userId)

      val winners = playersPlaying.filter(player => winnerUserIds.contains(player.getUserId()))

      winners
    

  extension(deck: Deck)

    /**
      * Returns a deck shuffled.
      *
      * @return a deck shuffles.
      */
    def shuffle() =
      Random.shuffle(deck)



