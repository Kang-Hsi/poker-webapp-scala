package apps.app77
import cs214.webapp.*
import cs214.webapp.server.{StateMachine}
import scala.util.{Random, Try}
import GamePhase.*

class Logic extends StateMachine[Event, State, View]:

  val appInfo: AppInfo = AppInfo(
    id = "app77",
    name = "TBD",
    description = "TBD",
    year = 2024
  )

  override val wire = Wire

  override def init(clients: Seq[UserId]): State =
    State(
      gamePhase = PreFlop,
      gameInfo = initGameInfo(clients),
      deck = Nil,
      gameConfig = initGameConfig()
    )
    // transitionState

  override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] =
    // the transitions returns a seq of action of states,
    // but we only need to display one state always , except when we transitiona round.
    // Or maybe also when we transition phase? Could be done, but for now no.
    //
    Try({

      val stateWithActionNaive = state.applyEventNaive(userId, event)

      // TODO need to verify if only one player in the thing or none
      // So that we skip to the river
      // here is the first  test for this:

      if stateWithActionNaive.gameInfo.getAllPlayingPlayers.length == 1
      then

        println("INFO : We are skipping to endRound because only one player is left")

        renderTheStates(
          stateWithActionNaive.goToEndRound()
        ) // TODO verify that the goToEndRoudn satifies all ; i did not checked
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

        // the only thing left is to rotate the players turn ig

        renderTheStates(Seq(stateWithActionNaive.rotatePlayerTurn()))
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
  private def renderTheStates(statesToRender: Seq[State]): Seq[Action[State]] =
    val renderStates = statesToRender.map(s => Action.Render(s))
    if renderStates.length == 2 then
      Seq(renderStates(0), Action.Pause(100), renderStates(1))
    else renderStates

