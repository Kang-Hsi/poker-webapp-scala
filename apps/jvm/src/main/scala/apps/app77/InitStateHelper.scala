package apps.app77

import cs214.webapp.*


/**
  * Get the Global Configuration for our game.
  **/
  val configuration = Configuration.get


  private val minPlayers = 3


  private def initGameInfo(clients: Seq[UserId]): GameInfo =
      GameInfo(
        initializePlayers(clients) ,//cards not shuffled watch out
        0,
        Nil,
        0,
        "Initialized game" :: Nil,
        0,
        0,
        0
      )
      
  private def initGameConfig(): GameConfig=
    GameConfig(
      configuration.getMaxRound,
      configuration.getSmallBlind,
      configuration.getBigBlind
    )

  

  /**
   * Gives the initial game state.
   * This game state is "virgin" : No deck is attibued, only the
   * player roles are.
   * This is done so that this game state can be passed trhough a general initRound function
  **/
  private def initVirginGameState(clients: Seq[UserId]):State=
    State(
      GamePhase.PreFlop,
      initGameInfo(clients),
      Nil,
      initGameConfig()
    )




  /**
   * Returns a List of Player Info from the set of User Id,
   * This list will already contain the initial roles of each player,
   * as well as the Status, the Money ,and of course the Id.
   * This list is ordered for the round before the first round : the first player is the dealer, 2nd the small blind, & third the big => On this ways we can easily call the method transition round to rotate effectively the list
  **/
  private def initializePlayers(clients: Seq[UserId]): List[PlayerInfo]=
    import Role.*
    assert(clients.length >= minPlayers, cs214.webapp.AppException("Not enough players ! Minimum is " + minPlayers ))

    val initialMoney = configuration.getInitialMoney

    (for
      (userId, position) <- clients.zipWithIndex
    yield
      if position == 0 then
        (userId, initialMoney, Dealer, Status.Playing, None, 0, false, initialMoney)
      else if position == 1 then
       (userId, initialMoney, SmallBlind, Status.Playing, None, 0, false, initialMoney)
      else if position == 2 then
        (userId, initialMoney, BigBlind, Status.Playing, None, 0, false, initialMoney)
      else 
        (userId, initialMoney, Normal, Status.Playing, None, 0, false, initialMoney)
    ).toList
