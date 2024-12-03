package apps.app77

import cs214.webapp.*
import cs214.webapp.server.{StateMachine}
import scala.util.{Random, Try}
import GamePhase.*
import CardHelper.*

class Logic extends StateMachine[Event, State, View]:


  val appInfo: AppInfo = AppInfo(
    id = "app77",
    name = "TBD",
    description = "TBD",
    year=2024
  )


  override val wire = Wire

  override def init(clients: Seq[UserId]): State = 
    State(
      gamePhase = PreFlop,
      gameInfo = initGameInfo(clients),
      deck = Nil,
      gameConfig = initGameConfig()
    ) 
    //transitionState

  



  override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] = ???
    

    

  override def project(state: State)(userId: UserId): View = 

    val gameInfoView = state.gameInfo
    val gameConfigView = state.gameConfig
    
    val players = gameInfoView.players

    val showOnlyUserCards = players.map(player =>
      if player.getUserId() != userId then
        player.withOptionHand(None) else
        player
    )

    View(
      gameInfoView.copy(players = showOnlyUserCards),
      gameConfigView
    )

      
      



