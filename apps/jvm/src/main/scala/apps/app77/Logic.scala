package apps.app77


import cs214.webapp.*
import cs214.webapp.server.{StateMachine}
import scala.util.{Random, Try}

class Logic extends StateMachine[Event, State, View]:

  
  val appInfo: AppInfo = AppInfo(
    id = "app77",
    name = "TBD",
    description = "TBD",
    year=2024
  )
  
  override val wire = Wire
  
  override def init(clients: Seq[UserId]): State = null

  override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] =null
  override def project(state: State)(userId: UserId): View = null
