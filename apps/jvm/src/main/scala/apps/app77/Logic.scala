package apps.app77
import cs214.webapp.*
import cs214.webapp.server.{StateMachine}
import scala.util.{Random, Try}
import GamePhase.*

class Logic extends StateMachine[Event, State, View]:

  val appInfo: AppInfo = AppInfo(
    id = "app77",
    name = "♤ ♡ Poker ♧ ♢",
    description = "Standard game of texas hold'em poker. (slightly modified)",
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

      val stateWithActionNaive = state.applyEventNaive(userId, event)

    

      println("DEBUG: hasEveryoneTalked: " + stateWithActionNaive.hasEveryoneTalked)
      println("DEBUG: hasEveryoneBettedSameAmount: " + stateWithActionNaive.hasEveryoneBettedSameAmount)

      if (stateWithActionNaive.gameInfo.getAllOnlyPlayingPlayers.length == 1 && stateWithActionNaive.hasEveryoneTalked) 
        || stateWithActionNaive.gameInfo.getAllOnlyPlayingPlayers.length == 0
      then

        println("INFO : We are skipping to endRound because only one player is left")
        
  
        val nbToSkip = 4 - stateWithActionNaive.gamePhase.ordinal 

        renderTheStates(
          transitionPhaseNTimes(Seq(stateWithActionNaive), nbToSkip)
        ) 
      else if stateWithActionNaive.hasEveryoneTalked &&
        stateWithActionNaive.hasEveryoneBettedSameAmount
      then
        println("DEBUG : Begin transitionning phase")
        val states = stateWithActionNaive.transitionPhase

        assert(states.length <= 2, "Not possible")

        if states.length == 1 then
          println("INFO : We are only transitionning a phase")
        if states.length == 2 then
          println("INFO : We are transitionning phase and round")

        renderTheStates(states)
      else
        println("DEBUG : Transitionning simple event")

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
      gameConfigView
    )

  /** Transform a sequence of states in a sequence of actions. If the number of
    * states in a sequence is two, it will add a pause
    */

  /**
    * Returns a sequence of actions given a sequence of states.
    *
    * @param statesToRender a sequence of states.
    * @return a sequence of actions.
    */
  private def renderTheStates(statesToRender: Seq[State]): Seq[Action[State]] =
    val renderStates = statesToRender.map(s => Action.Render(s))
    if renderStates.length == 2 then
      Seq(renderStates(0), Action.Pause(5000), renderStates(1))
    else renderStates
    
  private def transitionPhaseNTimes(state: Seq[State], times : Int):Seq[State]=
   if (times == 0) then
    state
   else
    transitionPhaseNTimes(state.head.transitionPhase , times - 1)  

  
