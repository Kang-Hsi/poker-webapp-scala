package apps.app77

import munit.FunSuite
import apps.app77.HandRank.evaluateHand


val suits = "♠♥♦♣"

class CardHelperTests extends FunSuite :

    test("allCards should contain 52 unique cards") :
        assert(CardHelper.allCards.size == 52, "Deck should have 52 cards")
        val uniqueCards = CardHelper.allCards.distinct
        assert(uniqueCards.size == 52, "All cards should be unique")
    /*
    test("the 15'th card of the deck should be a 2 of diamonds") :
        val expectedEmoji = "🃂"
        val expectedValue = 2
        val expectedSuite =  Suit.Diamond
        assert(CardHelper.allCards(14) == (expectedSuite, expectedValue, expectedEmoji), "There is a problem in the deck")
    */

    test("(A♦, Q♦, J♦, 9, 9) with (K♦, 10♦) should be RoyalFlush() "):
        val aceD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 14)(0)
        val queenD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 12)(0)
        val jackD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 11)(0)
        val nines = CardHelper.allCards.filter(card => card._2 == 9).take(2)

        val kingD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 13)(0)
        val tenD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 10)(0)

        val commCards = List(aceD, queenD, jackD) ++ nines

        val playerHand = List(kingD, tenD)
        assertEquals(evaluateHand(commCards ++ playerHand), RoyalFlush())

    test("(9♦, Q♦, J♦, 8♦, 8) with (K♦, 10♦) should be StraightFlush(13) "):
        val queenD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 12)(0)
        val jackD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 11)(0)
        val nineD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 9)(0)
        val eights = CardHelper.allCards.filter(card => (card._1 == Suit.Diamond || card._1 == Suit.Clubs) && card._2 == 8)
        val kingD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 13)(0)
        val tenD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 10)(0)

        val commCards = List(queenD, jackD, nineD) ++ eights
        val playerHand = List(kingD, tenD)
        assertEquals(evaluateHand(commCards ++ playerHand), StraightFlush(13))

    test("(9, 9, 4, 2, 3) with (9, 9) should be FourOfAKind(9, kicker4)") :
        val nines = CardHelper.allCards.filter(card => card._2 == 9)
        val four = CardHelper.allCards.filter(card => card._2 == 4)(0)
        val two = CardHelper.allCards.filter(card => card._2 == 2)(0)
        val three = CardHelper.allCards.filter(card => card._2 == 3)(0)

        val commCards = nines.take(2) ++ List(four, two, three)
        val playerHand = nines.drop(2).take(2)

        assertEquals(evaluateHand(commCards ++ playerHand), FourOfAKind(9, 4))

    test("(K,K,Q,Q,A) with (A,K) should be FullHouse(K(13), A(14))") :
        val kings = CardHelper.allCards.filter(card => card._2 == 13)
        val queens = CardHelper.allCards.filter(card => card._2 == 12)
        val aces = CardHelper.allCards.filter(card => card._2 == 14)
        val communalCards = kings.take(2) ++ queens.take(2) ++ aces.take(1)
        val playerHand = (kings.drop(2).take(1) ++ aces.drop(1).take(1)).toSet.toList
        assertEquals(evaluateHand(communalCards ++ playerHand), FullHouse(13, 14))

    test("(A♦, Q♦, J♦, 9, 9) with (3♦, 10♦) should be Flush(List(14,12,11,10,3)) "):
        val aceD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 14)(0)
        val queenD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 12)(0)
        val jackD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 11)(0)
        val nines = CardHelper.allCards.filter(card => card._1 != Suit.Diamond && card._2 == 9).take(2)

        val threeD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 3)(0)
        val tenD = CardHelper.allCards.filter(card => card._1 == Suit.Diamond && card._2 == 10)(0)

        val commCards = List(aceD, queenD, jackD) ++ nines
        val playerHand = List(threeD, tenD)

        assertEquals(evaluateHand(commCards ++ playerHand), Flush(List(14,12,11,10,3)))

    test("(A, 2, 5, 9, K) with (3, 4) should be Straight(5)"):
        val ace = CardHelper.allCards.filter(card => card._2 == 14)(0)
        val two = CardHelper.allCards.filter(card => card._2 == 2)(1)
        val five = CardHelper.allCards.filter(card => card._2 == 5)(2)
        val nine = CardHelper.allCards.filter(card => card._2 == 9)(0)
        val king = CardHelper.allCards.filter(card => card._2 == 13)(0)

        val three = CardHelper.allCards.filter(card => card._2 == 3)(3)
        val four = CardHelper.allCards.filter(card => card._2 == 4)(3)

        val commCards = List(ace, two, five, nine, king)
        val playerHand = List(three, four)

        assertEquals(evaluateHand(commCards ++ playerHand), Straight(5))

    test("(A, 2, 5, 9, K) with (5, 5) should be ThreeOfAKind(5, List(14, 13))"):
        val ace = CardHelper.allCards.filter(card => card._2 == 14)(0)
        val two = CardHelper.allCards.filter(card => card._2 == 2)(1)
        val fives = CardHelper.allCards.filter(card => card._2 == 5)
        val nine = CardHelper.allCards.filter(card => card._2 == 9)(0)
        val king = CardHelper.allCards.filter(card => card._2 == 13)(0)

        val commCards = List(ace, two, nine, king) ++ fives.take(1)
        val playerHand = fives.drop(1).take(2)

        assertEquals(evaluateHand(commCards ++ playerHand), ThreeOfAKind(5, List(14, 13)))

    test("(A, 2, 5, 9, K) with (5, 2) should be TwoPair(5, 2, kicker: A)"):

        val ace = CardHelper.allCards.filter(card => card._2 == 14)(0)
        val twos = CardHelper.allCards.filter(card => card._2 == 2)
        val fives = CardHelper.allCards.filter(card => card._2 == 5)
        val nine = CardHelper.allCards.filter(card => card._2 == 9)(3)
        val king = CardHelper.allCards.filter(card => card._2 == 13)(2)

        val commCards = List(ace, nine, king) ++ twos.take(1) ++ fives.take(1)
        val playerHand = twos.drop(1).take(1) ++ fives.drop(1).take(1)

        assertEquals(evaluateHand(commCards ++ playerHand), TwoPair(5, 2, 14))

    test("(A, 2, 5, 9, K) with (9, Q) should be OnePair(9, Kickers: A, K, Q)"):
        val ace = CardHelper.allCards.filter(card => card._2 == 14)(0)
        val two = CardHelper.allCards.filter(card => card._2 == 2)(1)
        val five = CardHelper.allCards.filter(card => card._2 == 5)(2)
        val nines = CardHelper.allCards.filter(card => card._2 == 9)
        val king = CardHelper.allCards.filter(card => card._2 == 13)(2)

        val queen = CardHelper.allCards.filter(card => card._2 == 12)(3)

        val commCards = List(ace, two, five, king) ++ nines.take(1)

        val playerHand = nines.drop(1).take(1) ++ List(queen)

        assertEquals(evaluateHand((commCards ++ playerHand)), OnePair(9, List(14,13,12)))

    test("(4, 2, 7, 8, 6) with (3, 10) should be HighCard(10,8,7,6,4)"):
        val four = CardHelper.allCards.filter(card => card._2 == 4)(2)
        val two = CardHelper.allCards.filter(card => card._2 == 2)(2)
        val seven = CardHelper.allCards.filter(card => card._2 == 7)(0)
        val eight = CardHelper.allCards.filter(card => card._2 == 8)(1)
        val six = CardHelper.allCards.filter(card => card._2 == 6)(3)

        val three = CardHelper.allCards.filter(card => card._2 == 3)(1)
        val ten = CardHelper.allCards.filter(card => card._2 == 10)(1)

        val commCards = List(four, two, seven, eight, six)

        val playerHand = List(three, ten)

        assertEquals(evaluateHand(commCards ++ playerHand), HighCard(List(10,8,7,6,4)))

    test("(10,8,7,6,4) is better than (10,7,6,4,5)"):
        assertEquals(HighCard(List(10,8,7,6,4)).compare(HighCard(List(10,7,6,4,5))), 1)