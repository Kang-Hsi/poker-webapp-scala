package apps.app77

import munit.FunSuite

class CardHelperTests extends FunSuite :

    test("allCards should contain 52 unique cards") :
        assert(CardHelper.allCards.size == 52, "Deck should have 52 cards")
        val uniqueCards = CardHelper.allCards.distinct
        assert(uniqueCards.size == 52, "All cards should be unique")

    test("the 15'th card of the deck should be a 2 of diamonds") :
        val expectedEmoji = "🃂"
        val expectedValue = 2
        val expectedSuite =  Suit.Diamond
        assert(CardHelper.allCards(14) == (expectedSuite, expectedValue, expectedEmoji), "There is a problem in the deck")








