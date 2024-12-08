package apps.app77

import cs214.webapp.*
import cs214.webapp.utils.WebappSuite
import apps.app77.CardHelper.{allCards, shuffle}

/** Example test suite for a generic web application using a state machine */
class Tests extends WebappSuite[Event, State, View]:

  // The specific state machine for your application needs to be provided here

  def provideSampleEvent(): Event = ???

  def provideSampleState(): View = ???

  def provideBadEvent(): Event = ???

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


  // Define some typical user scenarios and the initialization of state
  lazy val initialState: State = sm.init(USER_IDS)

  /** Test encoding and decoding of events */
  test("Encoding and decoding for bet event") {
    val betEvent = Event.Bet(100)
    println("BetEvent : " + betEvent)
    val json = wire.eventFormat.encode(betEvent)
    println("Encoded : " + json)
    val decode = wire.eventFormat.decode(json).get
    println("Decoded : " + decode)
    assert(decode == betEvent, s"Expected: $betEvent, got: $decode")
  }

  /** Test encoding and decoding of events */
  test("Encoding and decoding for check event") {
    val checkEvent = Event.Check()
    val json = wire.eventFormat.encode(checkEvent)
    println(json)
    val decode = wire.eventFormat.decode(json).get
    println(decode)
    assert(decode == checkEvent, s"Expected: $checkEvent, got: $decode")
  }

  test("Encoding and decoding for playersInfo with no cards") {
    val playerInfo = createInitialPlayerInfo(USER_IDS)
    //println("playyerInfo: "+ playerInfo(0))
    val json = viewFormatHelpers.encodePlayerInfo(playerInfo(0))
    //println("JSON playerInfo"+ json)
    val decode = viewFormatHelpers.decodePlayerInfo(json).get
    //println("decoded playerInfo" + decode)
    assertEquals(decode, playerInfo(0))
  }
  test("Decode option and encode option") {
    val hand = Some(Set((Suit.Heart, 10, "10 of heart"),(Suit.Diamond, 6, "6 of diamond")))
    //println("Hand : " + hand)
    val json = hand.encodeOption(viewFormatHelpers.encodeHand(_))
    //println("JSON : " + json)
    val decode = json.decodeOption(viewFormatHelpers.decodeHand(_)).get
    //println("Decode : " + decode)
    assertEquals(decode, hand)
  }
  test("Encoding and decoding for playersInfo with cards") {
    val playerInfo = createInitialPlayerInfo(USER_IDS)(0).withOptionHand(Some(Set((Suit.Heart, 10, "10 of heart"),(Suit.Diamond, 6, "6 of diamond"))))
    println("playyerInfo: "+ playerInfo)
    val json = viewFormatHelpers.encodePlayerInfo(playerInfo)
    println("JSON playerInfo"+ json)
    val decode = viewFormatHelpers.decodePlayerInfo(json).get
    println("decoded playerInfo" + decode)
    assertEquals(decode, playerInfo)
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
    val view = new View(gameInfo, gameConfig)
    println("View : " + view)
    val json = wire.viewFormat.encode(view)
    println("JSON view : " + json)
    val decode = wire.viewFormat.decode(json).get
    println("Decode : " + decode)
    assertEquals(decode, view)
  }
  test("Encoding & Decoding for gameInfo") {
    val playerInfo = createInitialPlayerInfo(USER_IDS)
    val gameInfo = createInitialGameInfo(playerInfo)
    //println("GameInfo view:**********")
    val json = viewFormatHelpers.encodeGameInfo(gameInfo)
    //println("JSON view : " + json)
    val decode = viewFormatHelpers.decodeGameInfo(json).get
    //println("Decode: " + decode)
    assertEquals(decode, gameInfo)

  }
  test("Encoding and decoding for view 2") {
    val playerInfo = createInitialPlayerInfo(USER_IDS)
    val gameInfo = createInitialGameInfo(playerInfo)
    val gameConfig = new GameConfig(maxRound = 20, smallBlind = 1, bigBlind = 2)
    val view = new View(gameInfo, gameConfig)
    //println("View : " + view)
    val json = wire.viewFormat.encode(view)
    //println("JSON view : " + json)
    val decode = wire.viewFormat.decode(json).get
    //println("Decode : " + decode)
    assertEquals(decode, view)
  }



/*
  /** Example test for initial state of the application */
  test("Initial state validation") {
    assert(initialState.isInstanceOf[State], "Initial state should be a valid State instance")
  }

  /** Test for typical user interaction */
  test("User interaction test") {
    val event = provideSampleEvent()
    val resultingActions = sm.transition(initialState)(UID0, event)
    assertSingleRender(resultingActions)
  }

  /** Test handling of invalid or exceptional cases */
  test("Exception handling") {
    val badEvent = provideBadEvent()
    assertFailure[Exception](sm.transition(initialState)(UID0, badEvent))
  }
*/

  /** Further tests for specific scenarios within your application */
  // TODO: Implement more specific tests based on actual application behavior
