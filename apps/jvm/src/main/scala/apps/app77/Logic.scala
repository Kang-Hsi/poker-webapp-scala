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
  override def project(state: State)(userId: UserId): View = 
    //testing different type of view 
    val cards = Set((Suit.Heart, 10, "10 of heart"),(Suit.Heart, 8, "8 of heart"))
    val players = List(
      new PlayerInfo("Will",1000, Role.SmallBlind, Status.Playing, Option(cards),0,false,100),
      new PlayerInfo("Basile",1000, Role.BigBlind, Status.Playing, Option(cards),0,false,100),
      new PlayerInfo("Francesco",1000, Role.Normal, Status.Playing, Option(cards),0,false,100),
      new PlayerInfo("Leo",1000, Role.Dealer, Status.Playing, Option(cards),0,false,100)
    )
    val communalCards = List((Suit.Heart, 4, "4 of heart"))
    val gameInfo = new GameInfo(players, 4, communalCards, 200, List("test"), 500, 10, 20)
    val gameConfig = new GameConfig(15, 10, 20)
    View(gameInfo, gameConfig)
