package apps.app77
import cs214.webapp.*
import cs214.webapp.server.{StateMachine}
import scala.util.{Random, Try}
import GamePhase.*

class Logic extends StateMachine[Event, State, View]:

  val appInfo: AppInfo = AppInfo(
    id = "app77",
    name = "♤ ♡ Poker ♧ ♢",
    description = "Standard game of texas hold'em poker.",
    year = 2024
  )

  override val wire = Wire

  override def init(clients: Seq[UserId]): State =
    State(
      gamePhase = EndRound,
      gameInfo = initGameInfo(clients),
      deck = Nil,
      gameConfig = initGameConfig()
    ).transitionRound()

  override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] =
    Try({

      state.gamePhase match

        case GamePhase.EndRound => throw IllegalMoveException("Please wait")

        case GamePhase.EndGame => 
          if event == Event.Restart() then
            Logger.info("Restarting the game")
            renderTheStates(Seq(init(state.gameInfo.players.map(_.getUserId()))))
          else
            throw IllegalMoveException("Game is ended!")

        case _ => 

          if (event == Event.Restart()) then
            throw IllegalMoveException("You cannot restart the game as the game is not finished.")

          val stateWithActionNaive = state.applyEventNaive(userId, event)

          Logger.debug("hasEveryoneTalked: " + stateWithActionNaive.hasEveryoneTalked)
          Logger.debug("hasEveryoneBettedSameAmount: " + stateWithActionNaive.hasEveryoneBettedSameAmount)
    
          if (stateWithActionNaive.gameInfo.getAllOnlyPlayingPlayers.length == 1 && stateWithActionNaive.hasEveryoneTalked) 
            || stateWithActionNaive.gameInfo.getAllOnlyPlayingPlayers.length == 0
          then
    
            Logger.info("We are skipping to endRound because only one player is left")
            
      
            val nbToSkip = 4 - stateWithActionNaive.gamePhase.ordinal 
    
            renderTheStates(
              transitionPhaseNTimes(Seq(stateWithActionNaive), nbToSkip)
            ) 
          else if stateWithActionNaive.hasEveryoneTalked &&
            stateWithActionNaive.hasEveryoneBettedSameAmount
          then
            Logger.debug("Begin transitionning phase")
            val states = stateWithActionNaive.transitionPhase
    
            require(states.length <= 2, "Not possible")
    
            if states.length == 1 then
              Logger.debug("We are only transitionning a phase")
            if states.length == 2 then
              Logger.debug("We are transitionning phase and round")
    
            renderTheStates(states)
          else
            Logger.debug("Transitionning simple event")
    
            renderTheStates(Seq(stateWithActionNaive))
        })
    
  override def project(state: State)(userId: UserId): View =

    val gameInfoView = state.gameInfo
    val gameConfigView = state.gameConfig

    val players = gameInfoView.players

    val showOnlyUserCards = players.map(player =>
      if player.getUserId() != userId then player.withOptionHand(None)
      else player
    )

    View(
      gameInfoView.copy(players = showOnlyUserCards),
      gameConfigView,
      state.gamePhase
    )


  /** Returns a sequence of actions given a sequence of states.
    *
    * @param statesToRender 
    *   a sequence of states.
    * @return 
    *   a sequence of actions.
    */
  private def renderTheStates(statesToRender: Seq[State]): Seq[Action[State]] =
    val renderStates = statesToRender.map(s => Action.Render(s))
    if renderStates.length == 2 then
      Seq(renderStates(0), Action.Pause(3000), renderStates(1))
    else renderStates
    

  /** Returns a sequence of states (transitioned n times).
    *
    * @param state 
    *   a sequence of state.
    * @param times 
    *   number of times to transition.
    * @return 
    *   a sequence of states transitioned n times.
    */
  private def transitionPhaseNTimes(state: Seq[State], times : Int):Seq[State]=
   if (times == 0) then
    state
   else
    transitionPhaseNTimes(state.head.transitionPhase , times - 1)  

  
