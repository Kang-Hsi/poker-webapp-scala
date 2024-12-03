package apps.app77

import munit.FunSuite
import cs214.webapp.UserId

class LogicTests extends FunSuite :

    test("isDealer should be true if called on a dealer") :
        val p = new PlayerInfo("player1", 0, Role.Dealer, Status.Playing, None, 0, true, 0)
        assert(p.isDealer() == true)

    test("isBigBlind should be true if called on the bigBlind") :
        val p = new PlayerInfo("player1", 0, Role.BigBlind, Status.Playing, None, 0, true, 0)
        assert(p.isBigBlind() == true)

    test("isSmallBlind should be true if called on the smallBlind") :
        val p = new PlayerInfo("player1", 0, Role.SmallBlind, Status.Playing, None, 0, true, 0)
        assert(p.isSmallBlind() == true)

    test("isNormal should be true if called on a normalPlayer") :
        val p = new PlayerInfo("player1", 0, Role.Normal, Status.Playing, None, 0, true, 0)
        assert(p.isNormal() == true)



