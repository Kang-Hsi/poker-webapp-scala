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
  def rotatePlayerTurnInternal(): GameInfo = //TODO check players that are not playing
    gameInfo.copy(players = {
      val oldPlayers = gameInfo.players
      val playersUpdated = oldPlayers.drop(1).appended(oldPlayers.head)
      println("DEBUG: rotatePlayerTurnInternal : " + playersUpdated)
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

    println("DEBUG: rotatePlayerRoles before players: " + players)

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

      println("DEBUG: rotatePlayerRoles after players (result): " + newPlayers)
    gameInfo.copy(players = newPlayers)

  /** Returns gameInfo with players role rotated. Simply put, new dealers, small
    * blind, big blind assigned. Makes sure to skip players not playing.
    *
    * @return
    *   gameInfo with players role rotated.
    */
  def rotatePlayerRolesInternal(): GameInfo =
    val players = gameInfo.players
    val playingPlayers = players.filter(player => player.getStatus() != Status.Spectating)
    val spectatingPlayers = players.diff(playingPlayers).map(player => player.withRole(Normal))
    val trickyRoles = List(Role.Dealer, Role.SmallBlind, Role.BigBlind)
    val normal = List.tabulate(playingPlayers.size - 3)(i => Role.Normal)
    val roles = trickyRoles ++ normal
    val queuePlayers = Queue(playingPlayers*)
    val queueRotated = queuePlayers.enqueue(queuePlayers.dequeue())
    val newPlayingPlayers = queueRotated.zip(roles).map((player,role) => player.withRole(role)).toList
    val newPlayers = newPlayingPlayers ++ spectatingPlayers
    gameInfo.copy(players = newPlayers).ensuring(gameInfo.players.size == players.size)

  /** Returns game info with players order of the round. For preflop player
    * after big blind starts. For flop, turn, river blind starts.
    *
    * @TODO
    *   since we execute blinds, begin of round is always small blind no?
    * @TODO
    *   players playing?
    * @return
    *   gameInfo with players order of the round.
    */
  def setBeginOfRoundOrderInternal(state: State): GameInfo =
    val players = gameInfo.players

    println("DEBUG: setBeginOfRoundOrder, players before set: " + players)

    val smallBlindPosition = players.indexWhere(player => player.isSmallBlind())
    val bigBlindPosition = players.indexWhere(player => player.isBigBlind())

    assert(smallBlindPosition >= 0, "No Small Blind?!")
    assert(bigBlindPosition >= 0, "No Big Blind?!")

    state.gamePhase match
      case PreFlop | EndRound => {
        val playersUpdated = 
          players.drop((bigBlindPosition + 1) % players.size) ++ players.take(
            (bigBlindPosition + 1) % players.size
          )
        
        println("DEBUG: SetBeginOfRoundOrderInternal after players at " + state.gamePhase + " : " + playersUpdated)

        gameInfo.copy(players = playersUpdated)

      }.ensuring(g => g.players.size == players.size, "Preflop | EndRound should not drop any players")

      case Flop | Turn | River  => {

        val playersUpdated =
          players.drop(smallBlindPosition) ++ players.take(smallBlindPosition)
      
        println("DEBUG: SetBeginOfRoundOrderInternal after players at " + state.gamePhase + " : " + playersUpdated)

        gameInfo.copy(players = playersUpdated)
        
          
        }.ensuring(g => g.players.size == players.size, "Flop | Turn |River should not drop any players")

      case EndGame =>
        throw Exception(
          "no order needed for endGame"
        ) // should never happen



  def createSidePots(): List[(List[UserId], Money)] =
    val players = gameInfo.players

    val contributions: List[(UserId, Money)] = players.map(player => (player.getUserId(), player.getPotContribution()))

    val sortedContributions = contributions.sortBy((_, contribution) => contribution)

    def newSidePot(remainingMoney: List[(UserId, Money)]): List[(List[UserId], Money)] =

      val zeroFiltered = remainingMoney.filter((_, contribution) => contribution > 0)

      if zeroFiltered.isEmpty then Nil else

        val smallestContr = zeroFiltered.head._2
        val thisPotPlayers = zeroFiltered.map((userId, _) => userId)

        val thisPotSize = smallestContr * zeroFiltered.size

        val remainingUpdated =
          remainingMoney.map((userId, contr) => if contr > 0 then (userId, contr - smallestContr) else (userId, contr) )

        (thisPotPlayers, thisPotSize) :: newSidePot(remainingUpdated.sortBy(_._2))

    newSidePot(sortedContributions)

  def distributePotInternal(): GameInfo =

    val sidePots = createSidePots()

    println("DEBUG: Side POTS " + sidePots)
    val players = gameInfo.players

    val communalCards = gameInfo.communalCards

    val winnersWithMoneyWon = sidePots.map(pot =>
      val (potPlayersId, potSize) = pot

      val playersInPot = players.filter(player => potPlayersId.contains(player.getUserId()))


      //find winner only considers players playing
      val winnersAndHand = CardHelper.findWinner(playersInPot, communalCards)
      val winners = winnersAndHand.map(w => w._1)
      
      println("DEBUG: Winner is " + winners)
      (winners.map(_.getUserId()), potSize)

    )

    val winnersAndHand = CardHelper.findWinner(players, communalCards)

    println("DEBUG: Winners with money won! " + winnersWithMoneyWon)
    
    val playersEarnings = winnersWithMoneyWon.foldLeft(Map[UserId, (Money, Option[HandRank])]())(
      (acc, winWithMoney) =>
        val (winnersId, potSize) = winWithMoney
        //case that there a no winners (a side pot where everyone folded)
        if winnersId.nonEmpty then
          val moneyWon = potSize / winnersId.size
          winnersId.foldLeft(acc)((newAcc, winnerId) =>
            newAcc.updated(winnerId, 
              (
                (newAcc.getOrElse(winnerId, (0,None))._1 + moneyWon), 
                winnersAndHand.find(
                    p => p._1.getUserId() == winnerId).get._2
            )
          )
        )
        else 
          acc
    )


    val playersUpdated = players.map(player =>
      val moneyWon = playersEarnings.getOrElse(player.getUserId(), (0,None))._1
      player.updateMoney(moneyWon)
    )
    gameInfo.copy(players = playersUpdated, pot = 0)
      .addCommunalCardsLog()
      .addWinningLogs(playersEarnings)



  def addWinningLogs(playersEarnings: Map[UserId,(Money, Option[HandRank])])=
    val winningLogs = playersEarnings.foldLeft(Nil)( (rest, e) => {
        val (user, (money, hand)) = e
        if money <= 0 then
          rest
        else
          if hand.isEmpty then
            (user + " won " + money + "$ !!!") :: rest
          else
            (user + " won " + money + "$ !!! With " + hand.get.toString) :: rest

    })

    gameInfo.copy(
      logs = gameInfo.logs ++ winningLogs
      )


  def addCommunalCardsLog():GameInfo=
    val communalCards = gameInfo.communalCards.map(
      c => c._2
    ).map(cardsToString)
    gameInfo.addLogGameInfo("Communal cards of the round : " + communalCards.mkString(", "))

  def addLogGameInfo(entry: String): GameInfo=
    val oldLogs = gameInfo.logs
    gameInfo.copy(
      logs = oldLogs :+ entry
    )
    


