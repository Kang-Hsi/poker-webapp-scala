package apps.app77

import cs214.webapp.*
import scala.util.boundary



trait HandRank extends Ordered[HandRank]: //same as Comparable java
  def rank: Int
  def compare(that: HandRank): Int = 
    val compareRank = rank.compare(that.rank)
    if compareRank != 0 then compareRank
    else compareTie(that)

  def compareTie(that: HandRank): Int


//cardNumbers to be in descending order!!!
case class HighCard(cardNumbers: List[Int]) extends HandRank {
  def rank: Int = 1

  def compareTie(that: HandRank): Int = 
    that match
      case HighCard(thatcardNumbers) => 
        compareHighCard(cardNumbers, thatcardNumbers)

      case _ => throw new IllegalArgumentException("HandRank are not the same?!") //smaller rank for any other higher hand combination
}

//kickers to be in descending order!!!
case class OnePair(pairNumber: Int, kickers: List[Int]) extends HandRank {
  def rank: Int = 2

  def compareTie(that: HandRank): Int = 
    that match
      case OnePair(otherPairNumber, otherKickers) =>
        val comparePairNumber = pairNumber.compare(otherPairNumber)
        if comparePairNumber != 0 then comparePairNumber else
          compareHighCard(kickers, otherKickers)
      
      case _ => throw new IllegalArgumentException("HandRank are not the same?!")
}

case class TwoPair(highPairNumber: Int, lowPairNumber: Int, kicker: Int) extends HandRank {
  def rank: Int = 3

  def compareTie(that: HandRank): Int = 
    that match
      case TwoPair(thatHighPairNumber, thatLowPairNumber, thatKicker) =>
        val compareHighPairNumber = highPairNumber.compare(thatHighPairNumber)
        if compareHighPairNumber != 0 then compareHighPairNumber else
          val compareLowPairNumber = lowPairNumber.compare(thatLowPairNumber)
          if compareLowPairNumber != 0 then compareLowPairNumber else
            kicker.compare(thatKicker)

      case _ => throw new IllegalArgumentException("HandRank are not the same?!")
}

//kickers to be in descending order
case class ThreeOfAKind(tripletNumber: Int, kickers: List[Int]) extends HandRank {
  def rank: Int = 4

  def compareTie(that: HandRank): Int =
    that match
      case ThreeOfAKind(thatTripletNumber, thatKickers) => 
        val compareTripletNumber = tripletNumber.compare(thatTripletNumber)
        if compareTripletNumber != 0 then compareTripletNumber else
          compareHighCard(kickers, thatKickers)
      
      case _ => throw new IllegalArgumentException("HandRank are not the same?!")
}

case class Straight(straightHighNumber: Int) extends HandRank {
  def rank: Int = 5

  def compareTie(that: HandRank): Int = 
    that match
      case Straight(otherStraightHighNumber: Int) =>
        straightHighNumber.compare(otherStraightHighNumber)
      
      case _ => throw new IllegalArgumentException("HandRank are not the same?!")
  
}

case class Flush(flushNumbers: List[Int]) extends HandRank {
  def rank: Int = 6

  def compareTie(that: HandRank): Int = 
    that match
      case Flush(otherFlushNumbers) =>
        compareHighCard(flushNumbers, otherFlushNumbers)
      
      case _ => throw new IllegalArgumentException("HandRank are not the same?!")
}

case class FullHouse(tripletNumber: Int, pairNumber: Int) extends HandRank {
  def rank: Int = 7

  def compareTie(that: HandRank): Int =
    that match
      case FullHouse(otherTripletNumber, otherPairNumber) =>
        val compareTripletNumber = tripletNumber.compare(otherTripletNumber)
        if compareTripletNumber != 0 then compareTripletNumber else
          pairNumber.compare(otherPairNumber)

      case _ => throw new IllegalArgumentException("HandRank are not the same?!")
}

case class FourOfAKind(quadNumber: Int, kicker: Int) extends HandRank {
  def rank: Int = 8

  def compareTie(that: HandRank): Int = 
    that match
      case FourOfAKind(otherQuadNumber, otherKicker: Int) =>
        val compareQuadNumber = quadNumber.compare(otherQuadNumber)
        if compareQuadNumber != 0 then compareQuadNumber else
          kicker.compare(otherKicker)

      case _ => throw new IllegalArgumentException("HandRank are not the same?!")
}

case class StraightFlush(straightFlushHighNumber: Int) extends HandRank {
  def rank: Int = 9

  def compareTie(that: HandRank): Int =
    that match
      case StraightFlush(otherStraightFlushHighNumber) =>
        straightFlushHighNumber.compare(otherStraightFlushHighNumber)

      case _ => throw new IllegalArgumentException("HandRank are not the same?!")
}

case class RoyalFlush() extends HandRank {
  def rank: Int = 10

  def compareTie(that: HandRank): Int = 
    that match
      case RoyalFlush() => 0
      case _ => throw new IllegalArgumentException("HandRank are not the same?!")
}
  
//make sure cardNumbers and thatcardNumbers are sorted in ascending order
def compareHighCard(cardNumbers: List[Int], thatcardNumbers: List[Int]): Int =
  (cardNumbers, thatcardNumbers) match
    case (h1 :: t1, h2 :: t2) =>
      val compareCardNum = h1.compare(h2)
      if compareCardNum != 0 then compareCardNum else
        compareHighCard(t1, t2)
    case(Nil, Nil) => 0
    case _ => throw Exception("Not possible, inputs should of same size.")



object HandRank: 
  /**
    * Returns a HandRank given a list of cards.
    *
    * @param cards player's hand & communal cards (7 cards).
    * @return a HandRank of the list of cards.
    */
  def evaluateHand(cards: List[Card]): HandRank = 
    val suits = cards.map((suit, _, _) => suit)
    val numbers = cards.map((_, number, _) => number)
    
//    val suitCount: Map[Suit, Int] = suits.groupBy(identity).view.mapValues(_.size).toMap
    val suitCount: Map[Suit,Int] = suits.groupBy(identity).map((suit,occurences) => (suit,occurences.size)) 
    val numbersCount: Map[Int, Int] = numbers.groupBy(identity).map((suit,occurences) => (suit,occurences.size))
    //same here

    val flushSuitOption: Option[Suit] = suitCount.find((_, count) => count >= 5).map((suit, _) => suit)

    val sortedNumbers = numbers.distinct.sorted.reverse //descending

    val tripletNumbers = numbersCount.filter((_, count) => count == 3).keys.toList.sorted.reverse
    val pairNumbers = numbersCount.filter((_, count) => count == 2).keys.toList.sorted.reverse

    //check from best to worst hand

    //Royal or Straight Flush
    flushSuitOption match 
      case Some(flushSuit) =>
        val flushCards = cards.filter((suit, _, _) => suit == flushSuit)
        val flushNumbers = flushCards.map((_, number, _) => number).sorted.reverse
        val flushWithMaybeAce = if flushNumbers.contains(14) then flushNumbers :+ 1 else flushNumbers

        findStraight(flushWithMaybeAce) match {
          case Some(highNumber) =>
            if highNumber == 14 then
              return RoyalFlush()
            else
              return StraightFlush(highNumber)
          case _ => 
        }

      case _ => 
    
    //Four of a kind
    numbersCount.find((_, count) => count == 4) match
      case Some((quadNumber, _)) =>
        val kicker = sortedNumbers.filter(_ != quadNumber).max
        return FourOfAKind(quadNumber, kicker)
    
      case _ =>

    //Full house
    if (tripletNumbers.nonEmpty && (pairNumbers.nonEmpty || tripletNumbers.size > 1)) then //case there are two tripletNumbers
      val tripletNumber = tripletNumbers.max
      val pairNumber = if tripletNumbers.size > 1 then tripletNumbers.min else pairNumbers.max
      return FullHouse(tripletNumber, pairNumber)

    //Flush
    flushSuitOption match
      case Some(flushSuit) =>
        val flushCards = cards.filter((suit, _, _) => suit == flushSuit)
        val flushNumbers = flushCards.map((_, number, _) => number).sorted.reverse

        return Flush(flushNumbers.take(5))
        
      case _ =>
    
    
    //Straight
    val sortedNumbersWithMaybeAce = if sortedNumbers.contains(14) then sortedNumbers :+ 1 else sortedNumbers
    findStraight(sortedNumbersWithMaybeAce) match
      case Some(highNumber) =>
        return Straight(highNumber)
      
      case _ => 

    //Three of a kind
    if tripletNumbers.size == 1 then
      val tripletNumber = tripletNumbers(0)
      val kickers = sortedNumbers.filter(_ != tripletNumber).take(2)
      return ThreeOfAKind(tripletNumber, kickers)
    

    //Two pairs
    if pairNumbers.size >= 2 then 
      val highPairNumber = pairNumbers(0)
      val lowPairNumber = pairNumbers(1)
      val kicker = sortedNumbers.filter(number => number != highPairNumber && number != lowPairNumber).max
      return TwoPair(highPairNumber, lowPairNumber, kicker)

    //One pair
    if pairNumbers.size == 1 then 
      val pairNumber = pairNumbers(0)
      val kickers = sortedNumbers.filter(number => number != pairNumber).take(3)
      return OnePair(pairNumber, kickers)


    //High card
    val highCards = sortedNumbers.take(5)
    return HighCard(highCards)

def findStraight(numbers: List[Int]): Option[Int] = 
    val straights = numbers.distinct.sorted.reverse.sliding(5) 

    straights.collectFirst{case possibleStraight if (possibleStraight(0) - possibleStraight(4) == 4) => possibleStraight(0)}
