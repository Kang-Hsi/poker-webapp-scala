package apps.app77

import apps.app77.GamePhase.*
import apps.app77.Role.*
import cs214.webapp.*
import scala.collection.mutable.Queue

extension (gameInfo: GameInfo)

  /** Returns list of players that are still playing (all in included).
    *
    * @return
    *   list of players that are still playing.
    */
  def getAllPlayingPlayers: List[PlayerInfo] =
    gameInfo.players.filter(player => player.isPlaying())

  /** Returns list of players that are only playing.
    *
    * @return
    *   list of players that are only playing.
    */
  def getAllOnlyPlayingPlayers: List[PlayerInfo] =
    gameInfo.players.filter(player => player.isOnlyPlaying())

  /** Returns gameInfo with players turn rotated.
    *
    * @return
    *   gameInfo with players turn rotated.
    */
  def rotatePlayerTurnInternal()
      : GameInfo = // TODO check players that are not playing
    gameInfo.copy(players = {
      val oldPlayers = gameInfo.players
      val playersUpdated = oldPlayers.drop(1).appended(oldPlayers.head)
      Logger.debug("rotatePlayerTurnInternal : " + playersUpdated)
      playersUpdated
    })

  /** Returns gameInfo with players role rotated. Simply put, new dealers, small
    * blind, big blind assigned. Makes sure to skip players not playing.
    *
    * @return
    *   gameInfo with players role rotated
    */
  def rotatePlayerRolesInternalV1(): GameInfo =

    val players = gameInfo.players

    Logger.debug("rotatePlayerRoles before players: " + players)

    require(players.size >= 3)
    val playersWithIndex = players.zipWithIndex

    val lastBigBlind = playersWithIndex.find((player, _) => player.isBigBlind())

    val newPlayers = lastBigBlind match
      case Some((bigBlindPlayer, indexBB)) =>
        val indexOfBigBlind = indexBB

        playersWithIndex.map((player, index) =>
          if player.isPlaying() then
            player.getRole() match
              case Dealer =>
                player.withRole(
                  players(Math.floorMod((index - 1), players.size)).getRole()
                )
              case SmallBlind => player.withRole(Dealer)
              case BigBlind   => player.withRole(SmallBlind)
              case Normal =>
                if index == ((indexOfBigBlind + 1) % players.length) then
                  player.withRole(BigBlind)
                else player
          else player
        )

      case None => throw Exception("No Big Blind in the game ??")

    Logger.debug(": rotatePlayerRoles after players (result): " + newPlayers)
    gameInfo.copy(players = newPlayers)

  /** Returns gameInfo with players role rotated. Simply put, new dealers, small
    * blind, big blind assigned. Makes sure to skip players not playing.
    *
    * @return
    *   gameInfo with players role rotated.
    */
  def rotatePlayerRolesInternal(): GameInfo =
    val players = gameInfo.players
    val playingPlayers =
      players.filter(player => player.getStatus() != Status.Spectating)
    val spectatingPlayers =
      players.diff(playingPlayers).map(player => player.withRole(Normal))
    val trickyRoles = List(Role.Dealer, Role.SmallBlind, Role.BigBlind)
    val normal = List.tabulate(playingPlayers.size - 3)(i => Role.Normal)
    val roles = trickyRoles ++ normal
    val queuePlayers = Queue(playingPlayers*)
    val queueRotated = queuePlayers.enqueue(queuePlayers.dequeue())
    val newPlayingPlayers = queueRotated
      .zip(roles)
      .map((player, role) => player.withRole(role))
      .toList
    val newPlayers = newPlayingPlayers ++ spectatingPlayers
    gameInfo
      .copy(players = newPlayers)
      .ensuring(gameInfo.players.size == players.size)

  /** Returns game info with players order of the round. For preflop player
    * after big blind starts. For flop, turn, river blind starts.
    *
    * @return
    *   gameInfo with players order of the round.
    */
  def setBeginOfRoundOrderInternal(state: State): GameInfo =
    val players = gameInfo.players

    Logger.debug("setBeginOfRoundOrder, players before set: " + players)

    val smallBlindPosition = players.indexWhere(player => player.isSmallBlind())
    val bigBlindPosition = players.indexWhere(player => player.isBigBlind())

    require(smallBlindPosition >= 0, "No Small Blind?!")
    require(bigBlindPosition >= 0, "No Big Blind?!")

    state.gamePhase match
      case PreFlop | EndRound =>
        {
          val playersUpdated =
            players.drop((bigBlindPosition + 1) % players.size) ++ players.take(
              (bigBlindPosition + 1) % players.size
            )

          Logger.debug(
            "SetBeginOfRoundOrderInternal after players at " + state.gamePhase + " : " + playersUpdated
          )

          gameInfo.copy(players = playersUpdated)

        }.ensuring(
          g => g.players.size == players.size,
          "Preflop | EndRound should not drop any players"
        )

      case Flop | Turn | River =>
        {

          val playersUpdated =
            players.drop(smallBlindPosition) ++ players.take(smallBlindPosition)

          Logger.debug(
            "SetBeginOfRoundOrderInternal after players at " + state.gamePhase + " : " + playersUpdated
          )

          gameInfo.copy(players = playersUpdated)

        }.ensuring(
          g => g.players.size == players.size,
          "Flop | Turn |River should not drop any players"
        )

      case EndGame =>
        throw Exception(
          "no order needed for endGame"
        )

  /** Returns a list of side pots with its size (money) and players who are part
    * of the pot.
    *
    * @return
    *   a list of side pots.
    */
  def createSidePots(): List[(List[UserId], Money)] =
    val players = gameInfo.players

    val contributions: List[(UserId, Money)] =
      players.map(player => (player.getUserId(), player.getPotContribution()))

    val sortedContributions =
      contributions.sortBy((_, contribution) => contribution)

    def newSidePot(
        remainingMoney: List[(UserId, Money)]
    ): List[(List[UserId], Money)] =

      val zeroFiltered =
        remainingMoney.filter((_, contribution) => contribution > 0)

      if zeroFiltered.isEmpty then Nil
      else

        val smallestContr = zeroFiltered.head._2
        val thisPotPlayers = zeroFiltered.map((userId, _) => userId)

        val thisPotSize = smallestContr * zeroFiltered.size

        val remainingUpdated =
          remainingMoney.map((userId, contr) =>
            if contr > 0 then (userId, contr - smallestContr)
            else (userId, contr)
          )

        (thisPotPlayers, thisPotSize) :: newSidePot(
          remainingUpdated.sortBy(_._2)
        )

    newSidePot(sortedContributions)

  /** Returns GameInfo updated with the pots distributed to the winner(s).
    *
    * @return
    *   GameInfo updated with the pots distributed.
    */
  def distributePotInternal(): GameInfo =

    val sidePots = createSidePots()

    Logger.debug("Side POTS " + sidePots)
    val players = gameInfo.players

    val communalCards = gameInfo.communalCards

    val winnersWithMoneyWonAndRank = sidePots.map(pot =>
      val (potPlayersId, potSize) = pot

      val playersInPot =
        players.filter(player => potPlayersId.contains(player.getUserId()))

      // find winner only considers players playing
      val winnersAndHand = CardHelper.findWinner(playersInPot, communalCards)
      val winners = winnersAndHand.map(w => w._1)

      Logger.debug("Winner is " + winners)
      (winnersAndHand.map(w => (w._1.getUserId(), w._2)), potSize)
    )


    Logger.debug("Winners with money won! " + winnersWithMoneyWonAndRank)

    val playersEarnings = winnersWithMoneyWonAndRank.foldLeft(
      Map[UserId, (Money, Option[HandRank])]()
    )((acc, winWithMoney) =>
      val (winnersId, potSize ) = winWithMoney
      // case that there a no winners (i.e a side pot where everyone folded)
      if winnersId.nonEmpty then
        val moneyWon = potSize / winnersId.size
        winnersId.foldLeft(acc)((newAcc, winnerId) =>
          newAcc.updated(
            winnerId._1,
            (
              (newAcc.getOrElse(winnerId._1, (0, None))._1 + moneyWon),
              winnerId._2
            )
          )
        )
      else acc
    )

    val playersUpdated = players.map(player =>
      val moneyWon = playersEarnings.getOrElse(player.getUserId(), (0, None))._1
      player.updateMoney(moneyWon)
    )
    gameInfo
      .copy(players = playersUpdated, pot = 0)
      .addCommunalCardsLog()
      .addWinningLogs(playersEarnings)

  /** Returns GameInfo with added logs for the winning player(s).
    *
    * @param playersEarnings
    *   a map of players earnings.
    * @return
    *   GameInfo with added logs for the winning player(s).
    */
  def addWinningLogs(
      playersEarnings: Map[UserId, (Money, Option[HandRank])]
  ): GameInfo =
    val winningLogs = playersEarnings.foldLeft(Nil)((rest, e) => {
      val (user, (money, hand)) = e
      if money <= 0 then rest
      else if hand.isEmpty then (user + " won " + money + "$ !!!") :: rest
      else (user + " won " + money + "$ !!! With " + hand.get.toString) :: rest

    })

    gameInfo.copy(
      logs = gameInfo.logs ++ winningLogs
    )

  /** Returns GameInfo with added log for the communal cards.
    *
    * @return
    */
  def addCommunalCardsLog(): GameInfo =
    val communalCards = gameInfo.communalCards.map(c => c._2).map(cardsToString)
    gameInfo.addLogGameInfo(
      "Communal cards of the round : " + communalCards.mkString(", ")
    )

  /** Returns GameInfo with an added log of the given entry.
    *
    * @param entry
    *   an entry (message).
    * @return
    *   GameInfo with an added log of the entry.
    */
  def addLogGameInfo(entry: String): GameInfo =
    val oldLogs = gameInfo.logs
    gameInfo.copy(
      logs = oldLogs :+ entry
    )
