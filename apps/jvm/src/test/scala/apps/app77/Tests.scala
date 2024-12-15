package apps.app77

import cs214.webapp.*
import cs214.webapp.utils.WebappSuite
import apps.app77.CardHelper.{allCards, shuffle}

/** Example test suite for a generic web application using a state machine */
class Tests extends WebappSuite[Event, State, View]:

  // The specific state machine for your application needs to be provided here

  val sm = Logic()
  val wire = sm.wire

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

  def createRandomPlayerInfo(playerIds: Seq[UserId])(using config : Configuration) : List[PlayerInfo] =
        playerIds.zipWithIndex.map { case (userId, index) =>
            val role = index match {
            case 0 => Role.Dealer
            case 1 => Role.SmallBlind
            case 2 => Role.BigBlind
            case _ => Role.Normal
            }
            val money = index match
              case 0 => 50
              case 1 => 30
              case 2 => config.getInitialMoney * 3 - 50 - 30
            (userId, config.getInitialMoney, role, Status.Playing, None: Option[PlayerHand], 0, false, 0)
        }.toList

  def createRandomGameInfo(players: List[PlayerInfo])(using config : Configuration): GameInfo =
      GameInfo(
          players = createRandomPlayerInfo(USER_IDS),
          roundNumber = 3,
          communalCards = List.empty,
          pot = 0,
          logs = List("Game initialized"),
          callAmount = 0,
          minRaise = 0,
          maxRaise = 0
      )

  def createRandomState(playerIds: Seq[UserId])(using config : Configuration): State =
      val playersInfo = createRandomPlayerInfo(playerIds)
      val gameInfo = createRandomGameInfo(playersInfo)
      val shuffledDeck = allCards.shuffle()
      State(
          gamePhase = GamePhase.PreFlop,
          gameInfo = gameInfo,
          deck = shuffledDeck,
          gameConfig = GameConfig(config.getMaxRound, config.getSmallBlind, config.getBigBlind))


  // Define some typical user scenarios and the initialization of state
  lazy val initialState: State = sm.init(USER_IDS)

  /** Test encoding and decoding of events */
  test("Encoding and decoding for bet event") {
    val betEvent = Event.Bet(100)
    //println("BetEvent : " + betEvent)
    val json = wire.eventFormat.encode(betEvent)
    //println("Encoded : " + json)
    val decode = wire.eventFormat.decode(json).get
    //println("Decoded : " + decode)
    assert(decode == betEvent, s"Expected: $betEvent, got: $decode")
  }

  /** Test encoding and decoding of events */
  test("Encoding and decoding for check event") {
    val checkEvent = Event.Check()
    val json = wire.eventFormat.encode(checkEvent)
    //println(json)
    val decode = wire.eventFormat.decode(json).get
    //println(decode)
    assert(decode == checkEvent, s"Expected: $checkEvent, got: $decode")
  }

  test("Encoding and decoding for playersInfo with no cards") {
    val playerInfo = createInitialPlayerInfo(USER_IDS)
    //println("playyerInfo: "+ playerInfo(0))
    val json = ViewFormatHelpers.encodePlayerInfo(playerInfo(0))
    //println("JSON playerInfo"+ json)
    val decode = ViewFormatHelpers.decodePlayerInfo(json).get
    //println("decoded playerInfo" + decode)
    assertEquals(decode, playerInfo(0))
  }
  test("Decode option and encode option") {
    val hand = Some(Set((Suit.Heart, 10, "10 of heart"),(Suit.Diamond, 6, "6 of diamond")))
    //println("Hand : " + hand)
    val json = hand.encodeOption(ViewFormatHelpers.encodeHand(_))
    //println("JSON : " + json)
    val decode = json.decodeOption(ViewFormatHelpers.decodeHand(_)).get
    //println("Decode : " + decode)
    assertEquals(decode, hand)
  }
  test("Encoding and decoding for playersInfo with cards") {
    val playerInfo = createInitialPlayerInfo(USER_IDS)(0).withOptionHand(Some(Set((Suit.Heart, 10, "10 of heart"),(Suit.Diamond, 6, "6 of diamond"))))
    //println("playyerInfo: "+ playerInfo)
    val json = ViewFormatHelpers.encodePlayerInfo(playerInfo)
    //println("JSON playerInfo"+ json)
    val decode = ViewFormatHelpers.decodePlayerInfo(json).get
    //println("decoded playerInfo" + decode)
    assertEquals(decode, playerInfo)
  }

  test("Encoding and decoding fro gamePhase") {
    val gamePhase = GamePhase.EndGame
    val json = ViewFormatHelpers.encodeGamePhase(gamePhase)
    val decode = ViewFormatHelpers.decodeGamePhase(json).get
    assertEquals(decode, gamePhase)
  }

   test("Encoding and decoding for view 1") {
      //testing different type of view
    val cards = Set((Suit.Heart, 10, "10 of heart"),(Suit.Heart, 8, "8 of heart"))
    val players = List(
      new PlayerInfo("2",1000, Role.SmallBlind, Status.Playing, Option(cards),0,false,100)
    )
    val communalCards = List((Suit.Heart, 4, "4 of heart"))
    val gameInfo = new GameInfo(players, 4, communalCards, 200, List("test"), 500, 10, 20)
    val gameConfig = new GameConfig(15, 10, 20)
    val gamePhase = GamePhase.EndRound
    val view = new View(gameInfo, gameConfig, gamePhase)
    //println("View : " + view)
    val json = wire.viewFormat.encode(view)
    //println("JSON view : " + json)
    val decode = wire.viewFormat.decode(json).get
    //println("Decode : " + decode)
    assertEquals(decode, view)
  }
  test("Encoding & Decoding for gameInfo") {
    val playerInfo = createInitialPlayerInfo(USER_IDS)
    val gameInfo = createInitialGameInfo(playerInfo)
    //println("GameInfo view:**********")
    val json = ViewFormatHelpers.encodeGameInfo(gameInfo)
    //println("JSON view : " + json)
    val decode = ViewFormatHelpers.decodeGameInfo(json).get
    //println("Decode: " + decode)
    assertEquals(decode, gameInfo)

  }
  test("Encoding and decoding for view 2") {
    val playerInfo = createInitialPlayerInfo(USER_IDS)
    val gameInfo = createInitialGameInfo(playerInfo)
    val gameConfig = new GameConfig(maxRound = 20, smallBlind = 1, bigBlind = 2)
    val gamePhase = GamePhase.EndRound
    val view = new View(gameInfo, gameConfig, gamePhase)
    //println("View : " + view)
    val json = wire.viewFormat.encode(view)
    //println("JSON view : " + json)
    val decode = wire.viewFormat.decode(json).get
    //println("Decode : " + decode)
    assertEquals(decode, view)
  }


  test("the first players has talked") {
    val users = USER_IDS
    val init = initialState
    val currentplayerID = initialState.gameInfo.players(0)._1
    val currentBetAmount = init.getCallAmount()
    val player1Talked = sm.transition(init)(initialState.gameInfo.players(0)._1, Event.Bet(currentBetAmount)).get(0) match
      case Action.Render(st) => st
      case _ => throw new IllegalStateException("Unexpected action type, expected Render")

    assert(player1Talked.gameInfo.players.find(p => p._1 == currentplayerID).get._7)
  }

  test("betting more then the current Bet Amount set the status to other player to : not haveTalked") {
    val users = USER_IDS
    val init = initialState
    val firstPlayerID = initialState.gameInfo.players(0)._1
    val secondPlayerID = initialState.gameInfo.players(1)._1
    val thirdPlayerID = initialState.gameInfo.players(2)._1
    val currentBetAmount = init.getCallAmount()
    val player1Talked =
      sm.transition(init)(firstPlayerID, Event.Bet(currentBetAmount - initialState.gameInfo.players.find(p => p._1 == firstPlayerID).get._6)).get(0) match
        case Action.Render(st) => st
        case _ => throw new IllegalStateException("Unexpected action type, expected Render")
    val player1And2havetalked =
      sm.transition(player1Talked)(secondPlayerID, Event.Bet(currentBetAmount - initialState.gameInfo.players.find(p => p._1 == secondPlayerID).get._6)).get(0) match
        case Action.Render(st) => st
        case _ => throw new IllegalStateException("Unexpected action type, expected Render")
    assert(player1And2havetalked.gameInfo.players.filter(p => p._7).size == 2)
    val thirdPlayerBetMore = sm.transition(player1And2havetalked)(thirdPlayerID, Event.Bet(10 + currentBetAmount - initialState.gameInfo.players.find(p => p._1 == thirdPlayerID).get._6)).get(0) match
      case Action.Render(st) => st
      case _ => throw new IllegalStateException("Unexpected action type, expected Render")
    assert(thirdPlayerBetMore.gameInfo.players.find(p => p._7).size == 1)
  }


  test("if everyone go AllIn with the same Amount we should go to endRound") {
    val users = USER_IDS
    val init = initialState
    val firstPlayerID = initialState.gameInfo.players(0)._1
    val secondPlayerID = initialState.gameInfo.players(1)._1
    val thirdPlayerID = initialState.gameInfo.players(2)._1
    val currentBetAmount = init.getCallAmount()
    val player1Talked =
      sm.transition(init)(firstPlayerID, Event.Bet(initialState.gameInfo.players.find(p => p._1 == firstPlayerID).get._2)).get(0) match
        case Action.Render(st) => st
        case _ => throw new IllegalStateException("Unexpected action type, expected Render")
    val player1And2havetalked =
      sm.transition(player1Talked)(secondPlayerID, Event.Bet(initialState.gameInfo.players.find(p => p._1 == secondPlayerID).get._2)).get(0) match
        case Action.Render(st) => st
        case _ => throw new IllegalStateException("Unexpected action type, expected Render")
    assert(sm.transition(player1And2havetalked)(thirdPlayerID, Event.Bet(initialState.gameInfo.players.find(p => p._1 == thirdPlayerID).get._2)).get.size == 3)
  }

  test("if someone goes AllIn his status should be AllIn after") {
    val state = createRandomState(USER_IDS)
    val playerID = state.gameInfo.players(0).getUserId()
    val money = state.gameInfo.players(0).getMoney()
    val player1GoesAllIn = sm.transition(state)(playerID, Event.Bet(money)).get(0) match
        case Action.Render(st) => st
        case _ => throw new IllegalStateException("Unexpected action type, expected Render")
    assert(player1GoesAllIn.gameInfo.players.find(p => p._1 == playerID).get._4 == Status.AllIn)
  }

