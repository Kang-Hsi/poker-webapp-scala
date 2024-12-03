package apps.app77

import munit.FunSuite
import apps.app77.CardHelper.{allCards, shuffle}
import cs214.webapp.UserId
import scala.util.Random
import scala.collection.mutable.Queue

class StateHelperTests extends FunSuite :

    def createInitialPlayerInfo(playerIds: Seq[UserId])(using config : Configuration) : List[PlayerInfo] =
        playerIds.zipWithIndex.map { case (userId, index) =>
            val role = index match {
            case 0 => Role.Dealer
            case 1 => Role.SmallBlind
            case 2 => Role.BigBlind
            case _ => Role.Normal
            }
            (userId, config.getInitialMoney, role, Status.Playing, None: Option[PlayerHand], 0, false, config.getInitialMoney)
        }.toList

    def createInitialGameInfo(players: List[PlayerInfo])(using config : Configuration): GameInfo =
        GameInfo(
            players = players,
            roundNumber = 0,
            communalCards = List.empty,
            pot = 0,
            logs = List("Game initialized"),
            callAmount = config.getSmallBlind,
            minRaise = config.getSmallBlind,
            maxRaise = config.getInitialMoney
        )

    def createInitialState(playerIds: Seq[UserId])(using config : Configuration): State =
        val playersInfo = createInitialPlayerInfo(playerIds)
        val gameInfo = createInitialGameInfo(playersInfo)
        val shuffledDeck = allCards.shuffle()
        State(
            gamePhase = GamePhase.PreFlop,
            gameInfo = gameInfo,
            deck = shuffledDeck,
            gameConfig = GameConfig(config.getMaxRound, config.getSmallBlind, config.getBigBlind)
        )

    def createUserIds(numPlayers: Int, prefix: String = "Player"): List[UserId] =
        List.tabulate(numPlayers)(i => s"$prefix$i")

    test("distributeCardsToPlayers should remove cards from the deck (we don't waist any card)") :
        val numberOfPlayers = 4
        val initialState = createInitialState(createUserIds(numPlayers = 4))
        val updatedState = initialState.distributeCardsToPlayers()
        val numberOfCardsDistributed = 2 * numberOfPlayers
        assert(updatedState.deck.size == initialState.deck.size - numberOfCardsDistributed)

    test("distributeCardsToPlayers: each player should have two cards in their hand") :
        val numberOfPlayers = 4
        val initialState = createInitialState(createUserIds(numPlayers = 4))
        val updatedState = initialState.distributeCardsToPlayers()
        assert(
            updatedState.gameInfo.players.forall(playerInfo => playerInfo._5.isDefined && playerInfo._5.get.size == 2)
        )

    test("rotatePlayerTurn : the number of players is remained after the rotation") :
        val numberOfPlayers = 4;
        val initialState = createInitialState(createUserIds(numPlayers = 4))
        val updatedState = initialState.rotatePlayerTurn()
        assert(updatedState.gameInfo.players.size == 4)

    test("rotatePlayerTurn : the first player should become last (the roles should not have changed yet)") :
        val numberOfPlayers = 4;
        val initialState = createInitialState(createUserIds(numPlayers = 4))
        val firstPlayerInit  = initialState.gameInfo.players(0)
        val updatedState = initialState.rotatePlayerTurn()
        val lastPlayerUpdated = updatedState.gameInfo.players(3)
        assert(firstPlayerInit == lastPlayerUpdated)

    test("rotatePlayerTurn : the second should become the first ") :
        val numberOfPlayers = 4;
        val initialState = createInitialState(createUserIds(numPlayers = 4))
        val secondPlayerInit  = initialState.gameInfo.players(1)
        val updatedState = initialState.rotatePlayerTurn()
        val firstPlayerUpdated = updatedState.gameInfo.players(0)
        assert(secondPlayerInit == firstPlayerUpdated)

    test("rotatePlayerTurn: the third should become the second") :
        val initialState = createInitialState(createUserIds(numPlayers = 4))
        val thirdPlayerInit = initialState.gameInfo.players(2)
        val updatedState = initialState.rotatePlayerTurn()
        val secondPlayerUpdated = updatedState.gameInfo.players(1)
        assert(thirdPlayerInit == secondPlayerUpdated, "The third player should now be the second player.")

    test("rotatePlayerTurn: the last should become the third") :
        val initialState = createInitialState(createUserIds(numPlayers = 4))
        val lastPlayerInit = initialState.gameInfo.players(3)
        val updatedState = initialState.rotatePlayerTurn()
        val thirdPlayerUpdated = updatedState.gameInfo.players(2)
        assert(lastPlayerInit == thirdPlayerUpdated, "The last player should now be the third player.")

    test("rotatePlayerRole: the dealer should become Normal (with 4 players)") :
        var initialState = createInitialState(createUserIds(numPlayers = 4))
        var dealer = initialState.gameInfo.players.find(p => p.isDealer()).get
        for (_ <- 0 to 3)
            val updatedState = initialState.rotatePlayerRole()
            val normal = updatedState.gameInfo.players.find(player => player.isNormal()).get
            assert(dealer._1 == normal._1, "Dealer should have become a normal player")
            initialState = updatedState
            dealer = initialState.gameInfo.players.find(p => p._3 == Role.Dealer).get

    test("rotatePlayerRole: Small Blind should become Dealer after full rotation") :
        var initialState = createInitialState(createUserIds(numPlayers = 4))
        var smallBlind = initialState.gameInfo.players.find(p => p.isSmallBlind()).get
        for (_ <- 0 to 3) {
            var updatedState = initialState.rotatePlayerRole()
            var dealer = updatedState.gameInfo.players.find(player => player.isDealer()).get
            assert(smallBlind._1 == dealer._1, "Small Blind should have become the Dealer after full rotation")
            initialState = updatedState
            smallBlind = initialState.gameInfo.players.find(p => p.isSmallBlind()).get
    }

    test("rotatePlayerRole: Big Blind should become Small Blid after full rotation") :
        var initalState = createInitialState(createUserIds(numPlayers = 4))
        var bigBlind = initalState.gameInfo.players.find(p => p.isBigBlind()).get

        for (_ <- 0 to 3) {
            var updatedState = initalState.rotatePlayerRole()
            var smallBlind = updatedState.gameInfo.players.find(player => player.isSmallBlind()).get
            assert(bigBlind._1 == smallBlind._1, "Big Blind should have become Small Blind after full rotation")
            initalState = updatedState
            bigBlind = initalState.gameInfo.players.find(p => p.isBigBlind()).get
        }

    test("rotatePlayerRole: Normal player should become Big Blind after full rotation") :
        var initalState = createInitialState(createUserIds(numPlayers = 4))
        var normal = initalState.gameInfo.players.find(p => p.isNormal()).get

        for (_ <- 0 to 3) {
            var updatedState = initalState.rotatePlayerRole()
            var bigBlind = updatedState.gameInfo.players.find(player => player.isBigBlind()).get
            assert(normal._1 == bigBlind._1, "Normal player should have become big blind after full rotation")
            initalState = updatedState
            normal = initalState.gameInfo.players.find(p => p.isNormal()).get
        }

    test("rotatePlayerRole: The rotation is handled properly with a random number of players (2 < x < 7)") :
        val numberOfPlayers = Random.between(3, 7)
        val trickyRoles = List(Role.Dealer, Role.SmallBlind, Role.BigBlind)
        val normal = List.tabulate(numberOfPlayers - 3)(i => Role.Normal)
        val roles = trickyRoles ++ normal

        val listIDs = createUserIds(numberOfPlayers).toSeq
        val queueIDs = Queue(listIDs)

        val initalState = createInitialState(listIDs)

        val updatedState = initalState.rotatePlayerRole()
        val mapIdRoleUpdated = updatedState.gameInfo.players.map(playerInfo => (playerInfo._1, playerInfo._3))

        val queueIDsRotated = queueIDs.enqueue(queueIDs.dequeue())
        val mapIDRoleQueue  = queueIDsRotated.zip(roles)

        assert(mapIDRoleQueue == mapIdRoleUpdated)






