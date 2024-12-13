package apps.app77

import cs214.webapp.*

trait HandRank extends Ordered[HandRank]: // same as Comparable java
  /** Returns rank number of a HandRank.
    *
    * @return
    *   rank number.
    */
  def rank: Int

  override def compare(that: HandRank): Int =
    val compareRank = rank.compare(that.rank)
    if compareRank != 0 then compareRank
    else compareTie(that)

  /** Compares this HandRank with another.
    *
    * @param that
    *   a HandRank.
    * @return
    *   - -1 if this HandRank < thatHandRank
    *   - 0 if this HandRank == thatHandRank
    *   - 1 if this HandRank > thatHandRank
    */
  def compareTie(that: HandRank): Int

case class HighCard(cardNumbers: List[Int]) extends HandRank {
  override def rank: Int = 1

  override def compareTie(that: HandRank): Int =
    that match
      case HighCard(thatcardNumbers) =>
        compareHighCard(cardNumbers, thatcardNumbers)

      case _ =>
        throw new IllegalArgumentException(
          "HandRank are not the same?!"
        ) 

  override def toString(): String =
    "high cards " + cardNumbers.map(cardsToString(_)).mkString(", ")
}

case class OnePair(pairNumber: Int, kickers: List[Int]) extends HandRank {
  override def rank: Int = 2

  override def compareTie(that: HandRank): Int =
    that match
      case OnePair(otherPairNumber, otherKickers) =>
        val comparePairNumber = pairNumber.compare(otherPairNumber)
        if comparePairNumber != 0 then comparePairNumber
        else compareHighCard(kickers, otherKickers)

      case _ =>
        throw new IllegalArgumentException("HandRank are not the same?!")

  override def toString(): String = "pairs of " + cardsToString(
    pairNumber
  ) + ", kickers " + kickers.map(cardsToString(_)).mkString(", ")
}

case class TwoPair(highPairNumber: Int, lowPairNumber: Int, kicker: Int)
    extends HandRank {
  override def rank: Int = 3

  override def compareTie(that: HandRank): Int =
    that match
      case TwoPair(thatHighPairNumber, thatLowPairNumber, thatKicker) =>
        val compareHighPairNumber = highPairNumber.compare(thatHighPairNumber)
        if compareHighPairNumber != 0 then compareHighPairNumber
        else
          val compareLowPairNumber = lowPairNumber.compare(thatLowPairNumber)
          if compareLowPairNumber != 0 then compareLowPairNumber
          else kicker.compare(thatKicker)

      case _ =>
        throw new IllegalArgumentException("HandRank are not the same?!")

  override def toString(): String =
    "two pairs " + cardsToString(highPairNumber) + " and " + cardsToString(
      lowPairNumber
    ) + ", kicker " + cardsToString(kicker)
}


case class ThreeOfAKind(tripletNumber: Int, kickers: List[Int])
    extends HandRank {
  override def rank: Int = 4

  override def compareTie(that: HandRank): Int =
    that match
      case ThreeOfAKind(thatTripletNumber, thatKickers) =>
        val compareTripletNumber = tripletNumber.compare(thatTripletNumber)
        if compareTripletNumber != 0 then compareTripletNumber
        else compareHighCard(kickers, thatKickers)

      case _ =>
        throw new IllegalArgumentException("HandRank are not the same?!")

  override def toString(): String = "three of a kind of " + cardsToString(
    tripletNumber
  ) + ", kickers" + kickers.map(cardsToString(_)).mkString(", ")
}

case class Straight(straightHighNumber: Int) extends HandRank {
  override def rank: Int = 5

  override def compareTie(that: HandRank): Int =
    that match
      case Straight(otherStraightHighNumber: Int) =>
        straightHighNumber.compare(otherStraightHighNumber)

      case _ =>
        throw new IllegalArgumentException("HandRank are not the same?!")

  override def toString(): String =
    "straight ending with " + cardsToString(straightHighNumber)
}

case class Flush(flushNumbers: List[Int], flushSuit: Suit) extends HandRank {
  override def rank: Int = 6

  override def compareTie(that: HandRank): Int =
    that match
      case Flush(otherFlushNumbers, _) =>
        compareHighCard(flushNumbers, otherFlushNumbers)

      case _ =>
        throw new IllegalArgumentException("HandRank are not the same?!")

  override def toString(): String = "flush of " + flushSuit.toString().toLowerCase() +"s \n " + flushNumbers.map(cardsToString(_)).mkString(", ")
}

case class FullHouse(tripletNumber: Int, pairNumber: Int) extends HandRank {
  override def rank: Int = 7

  override def compareTie(that: HandRank): Int =
    that match
      case FullHouse(otherTripletNumber, otherPairNumber) =>
        val compareTripletNumber = tripletNumber.compare(otherTripletNumber)
        if compareTripletNumber != 0 then compareTripletNumber
        else pairNumber.compare(otherPairNumber)

      case _ =>
        throw new IllegalArgumentException("HandRank are not the same?!")

  override def toString(): String = "full house, three of " + cardsToString(
    tripletNumber
  ) + " two of " + cardsToString(pairNumber)
}

case class FourOfAKind(quadNumber: Int, kicker: Int) extends HandRank {
  override def rank: Int = 8

  override def compareTie(that: HandRank): Int =
    that match
      case FourOfAKind(otherQuadNumber, otherKicker: Int) =>
        val compareQuadNumber = quadNumber.compare(otherQuadNumber)
        if compareQuadNumber != 0 then compareQuadNumber
        else kicker.compare(otherKicker)

      case _ =>
        throw new IllegalArgumentException("HandRank are not the same?!")

  override def toString(): String = "four of a kind of " + cardsToString(
    quadNumber
  ) + ", kicker " + cardsToString(kicker)
}

case class StraightFlush(straightFlushHighNumber: Int, flushSuit: Suit) extends HandRank {
  override def rank: Int = 9

  override def compareTie(that: HandRank): Int =
    that match
      case StraightFlush(otherStraightFlushHighNumber, _) =>
        straightFlushHighNumber.compare(otherStraightFlushHighNumber)

      case _ =>
        throw new IllegalArgumentException("HandRank are not the same?!")

  override def toString(): String =
    "straight flush of " + flushSuit.toString().toLowerCase() + "s ending with " + cardsToString(straightFlushHighNumber)
}

case class RoyalFlush(flushSuit: Suit) extends HandRank {
  override def rank: Int = 10 

  override def compareTie(that: HandRank): Int =
    that match
      case RoyalFlush(_) => 0
      case _ =>
        throw new IllegalArgumentException("HandRank are not the same?!")

  override def toString(): String = " royal flush of " + flushSuit.toString().toLowerCase() + "s"
}

/** Compares a list of cards by another list of cards
  *
  * Note: The lists are in descending order.
  *
  * @param cardNumbers
  *   a list of card numbers.
  * @param thatcardNumbers
  *   that list of card numbers.
  * @return
  *   - -1 if this cardNumbers < thatCardNumbers
  *   - 0 if this cardNumbers == thatCardNumbers
  *   - 1 if this cardNumbers > thatCardNumbers
  */
def compareHighCard(cardNumbers: List[Int], thatcardNumbers: List[Int]): Int =
  (cardNumbers, thatcardNumbers) match
    case (h1 :: t1, h2 :: t2) =>
      val compareCardNum = h1.compare(h2)
      if compareCardNum != 0 then compareCardNum else compareHighCard(t1, t2)
    case (Nil, Nil) => 0
    case _ => throw Exception("Not possible, inputs should of same size.")

/** Returns string "value" of a card.
  *
  * Especially useful for face cards and ace.
  *
  * @param card
  *   a card (int) value.
  * @return
  *   string of a card.
  */
def cardsToString(card: Int): String =
  val stringValue = List(
    "",
    "A",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "J",
    "Q",
    "K",
    "A"
  )

  stringValue(card)

object HandRank:
  /** Returns a HandRank given a list of cards.
    *
    * @param cards
    *   player's hand & communal cards (7 cards).
    * @return
    *   a HandRank of the list of cards.
    */
  def evaluateHand(cards: List[Card]): HandRank =
    val suits = cards.map((suit, _, _) => suit)
    val numbers = cards.map((_, number, _) => number)

    val suitCount: Map[Suit, Int] =
      suits.groupBy(identity).map((suit, occurences) => (suit, occurences.size))
    val numbersCount: Map[Int, Int] = numbers
      .groupBy(identity)
      .map((suit, occurences) => (suit, occurences.size))

    val flushSuitOption: Option[Suit] =
      suitCount.find((_, count) => count >= 5).map((suit, _) => suit)

    val sortedNumbers = numbers.distinct.sorted.reverse // descending

    val tripletNumbers =
      numbersCount.filter((_, count) => count == 3).keys.toList.sorted.reverse
    val pairNumbers =
      numbersCount.filter((_, count) => count == 2).keys.toList.sorted.reverse

    // check from best to worst hand

    // Royal or Straight Flush
    flushSuitOption match
      case Some(flushSuit) =>
        val flushCards = cards.filter((suit, _, _) => suit == flushSuit)
        val flushNumbers =
          flushCards.map((_, number, _) => number).sorted.reverse
        val flushWithMaybeAce =
          if flushNumbers.contains(14) then flushNumbers :+ 1 else flushNumbers

        findStraight(flushWithMaybeAce) match {
          case Some(highNumber) =>
            if highNumber == 14 then return RoyalFlush(flushSuit)
            else return StraightFlush(highNumber, flushSuit)
          case _ =>
        }

      case _ =>

    // Four of a kind
    numbersCount.find((_, count) => count == 4) match
      case Some((quadNumber, _)) =>
        val kicker = sortedNumbers.filter(_ != quadNumber).max
        return FourOfAKind(quadNumber, kicker)

      case _ =>

    // Full house
    if (
      tripletNumbers.nonEmpty && (pairNumbers.nonEmpty || tripletNumbers.size > 1)
    ) then // case there are two tripletNumbers
      val tripletNumber = tripletNumbers.max
      val pairNumber =
        if tripletNumbers.size > 1 then tripletNumbers.min else pairNumbers.max
      return FullHouse(tripletNumber, pairNumber)

    // Flush
    flushSuitOption match
      case Some(flushSuit) =>
        val flushCards = cards.filter((suit, _, _) => suit == flushSuit)
        val flushNumbers =
          flushCards.map((_, number, _) => number).sorted.reverse

        return Flush(flushNumbers.take(5), flushSuit)

      case _ =>

    // Straight
    val sortedNumbersWithMaybeAce =
      if sortedNumbers.contains(14) then sortedNumbers :+ 1 else sortedNumbers
    findStraight(sortedNumbersWithMaybeAce) match
      case Some(highNumber) =>
        return Straight(highNumber)

      case _ =>

    // Three of a kind
    if tripletNumbers.size == 1 then
      val tripletNumber = tripletNumbers(0)
      val kickers = sortedNumbers.filter(_ != tripletNumber).take(2)
      return ThreeOfAKind(tripletNumber, kickers)

    // Two pairs
    if pairNumbers.size >= 2 then
      val highPairNumber = pairNumbers(0)
      val lowPairNumber = pairNumbers(1)
      val kicker = sortedNumbers
        .filter(number => number != highPairNumber && number != lowPairNumber)
        .max
      return TwoPair(highPairNumber, lowPairNumber, kicker)

    // One pair
    if pairNumbers.size == 1 then
      val pairNumber = pairNumbers(0)
      val kickers = sortedNumbers.filter(number => number != pairNumber).take(3)
      return OnePair(pairNumber, kickers)

    // High card
    val highCards = sortedNumbers.take(5)
    return HighCard(highCards)

/** Returns ending number of a straight if found in a list of numbers, else
  * None.
  *
  * @param numbers
  *   list of a numbers.
  * @return
  *   ending number of a straight if found, else None.
  */
def findStraight(numbers: List[Int]): Option[Int] =
  val straights = numbers.distinct.sorted.reverse.sliding(5)
  straights.collectFirst {
    case possibleStraight if (possibleStraight(0) - possibleStraight(4) == 4) =>
      possibleStraight(0)
  }
