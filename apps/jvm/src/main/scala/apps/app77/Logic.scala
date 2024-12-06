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
    val cards = Set((Suit.Heart, 10, s"${CardHelper.allRepresentations(8)}"),(Suit.Heart, 8, s"${CardHelper.allRepresentations(9)}"))
    val players = List(
      new PlayerInfo("Will",1000, Role.SmallBlind, Status.Playing, Option(cards),0,false,100),
      new PlayerInfo("Basile",1000, Role.BigBlind, Status.Playing, None,0,false,100),
      new PlayerInfo("Francesco",1000, Role.Normal, Status.Playing, None,0,false,100),
      new PlayerInfo("Leo",1000, Role.Dealer, Status.Playing, None,0,false,100)
    )
    val communalCards = List((Suit.Heart, 4, s"${CardHelper.allRepresentations(4)}"))
    val log =   List(
    "The game has started!",
    "Will placed the Small Blind (10$)",
    "Basile placed the Big Blind (20$)",
    "Francesco called (20$)",
    "Leo raised to 50$",
    "Will folded",
    "Basile called the raise (50$)",
    "Francesco checked",
    "The flop: 4♥, 10♦, K♣",
    "Basile bet 100$",
    "Francesco raised to 200$",
    "Leo folded",
    "Basile called (200$)",
    "The turn: 8♠",
    "Francesco checked",
    "Basile bet 300$",
    "Francesco called (300$)",
    "The river: A♦",
    "Basile went all-in with 500$",
    "Francesco folded",
    "Basile won the pot of 2000$"
  )
    val gameInfo = new GameInfo(players, 4, communalCards, 200, log, 500, 10, 20)
    val gameConfig = new GameConfig(15, 10, 20)
    View(gameInfo, gameConfig)
