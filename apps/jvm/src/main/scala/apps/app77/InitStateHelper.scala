package apps.app77

import cs214.webapp.*
import Role.*


/**
  * Get the Global Configuration for our game.
  **/
  val configuration = Configuration.get

  /* Minimum amount of players. */
  private val minPlayers = 3

  /**
    * Returns gameInfo initialized.
    *
    * @param clients a list of clients (players).
    * @return gameInfo initialized.
    */
  private def initGameInfo(clients: Seq[UserId]): GameInfo =
      GameInfo(
        initializePlayers(clients) ,
        0,
        Nil,
        0,
        "Initialized game" :: Nil,
        0,
        0,
        0
      )
      
  /**
    * Returns GameConfig initialized.
    *
    * @return GameConfig initialized.
    */
  private def initGameConfig(): GameConfig=
    GameConfig(
      configuration.getMaxRound,
      configuration.getSmallBlind,
      configuration.getBigBlind
    )

  /**
    * Returns list of players initialized.
    *
    * @param clients a list of clients (players).
    * @return list of players initialized.
    */
  private def initializePlayers(clients: Seq[UserId]): List[PlayerInfo]=

    require(clients.length >= minPlayers, "Minimum players is 3 :-)")

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
