package apps.app77
package poker

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
