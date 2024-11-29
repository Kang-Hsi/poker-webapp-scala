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
  
  private val minPlayers = 3

    /**
   * Get the Global Configuration for our game
  **/
  val conf = Configuration.get() 

  override val wire = Wire
  
  override def init(clients: Seq[UserId]): State = ???

  override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] =   ??? 

  override def project(state: State)(userId: UserId): View = ???

  
  private def initGameConfig(): GameConfig=
    GameConfig(
      conf.getRounds()
    )

  private def initGameInfo(clients: Seq[UserId]): GameInfo=
    GameInfo(
      createFirstPlayerInfo(clients) ,//cards not shuffled watch out
      0,
      Nil,
      0,
      "Initialized game" :: Nil,
      0,
      0,
      0
    )



/**
 * Method to be called in beetween rounds
 * Does several things : 
 *  1 - rotates the order of players
 *  2 - get a new deck
 *  3 - reshuffle the deck
 *  4 - assigns cards to each player
 *  5 - finds who won, (not sure about this : We need to call this method at the beginnign of the game) 
 *  6 - updates the balance of the player who won
 *  7 - updates the blind amount if needed
**/
 private def transitionRound(state:State):State=
    ???

  /**
   * Returns a List of Player Info from the set of User Id,
   * This list will already contain the initial roles of each player,
   * as well as the Status, the Money ,and of course the Id.
   * This list is ordered for the round before the first round : the first player is the dealer, 2nd the small blind, & third the big => On this ways we can easily call the method transition round to rotate effectively the list
  **/
  private def createFirstPlayerInfo(clients: Seq[UserId]): List[PlayerInfo]=
    assert(clients.length >= minPlayers, cs214.webapp.AppException("Not enough players ! Minimum is " + minPlayers ))

    (for
      user <- clients.zipWithIndex
    yield
      if user._2 == 1 then 
       (user._1, conf.getMoney(), Role.SmallBlind(conf.getBlindTruc), 
         Status.Playing, None, 0)
      else if user._2 == 2 then
        (user._1, conf.getMoney(), Role.BigBlind(conf.getBlindTruc), 
         Status.Playing, None, 0)
      else if user._2 == 0 then
        (user._1, conf.getMoney(), Role.Dealer, 
         Status.Playing, None, 0)
      else
        (user._1, conf.getMoney(), Role.Normal, 
         Status.Playing, None, 0)
    ).toList


        
    
/**
 * Some usefull methods for easy manipulation of PlayerInfo type
**/
extension (p :PlayerInfo)
  def withId(id:UserId)=
    (id, p._2, p._3, p._4, p._5, p._6)

  def withMoney(money :Money)=
    (p._1, money, p._3, p._4, p._5, p._6)

  def withRole(role: Role)=
    (p._1, p._2, role, p._4, p._5, p._6)

  def withStatus(status :Status)=
    (p._1, p._2, p._3, status, p._5, p._6)

  def withOptionHand(hand: Option[PlayerHand])=
    (p._1, p._2, p._3, p._4, hand, p._6)

  def withBetAmount(betAmount: BetAmount)=
    (p._1, p._2, p._3, p._4, p._5, betAmount)

  def updateMoney(moneyToAddOrSub: Money)=
    p.withMoney(p._2 + moneyToAddOrSub)

  def updateBetAmount(betAmountToAddOrSub : BetAmount)=
    p.withBetAmount(p._6 + betAmountToAddOrSub)

  def fold()=
    p.withStatus(Status.Spectating) //watch out , we might need to reset the betAmount

  def getRole()=
    p._3

  def isDealer()=
    p.getRole() == Role.Dealer

  def isSmallBlind()=
    p.getRole() == Role.SmallBlind

  def isBigBlind()=
    p.getRole() == Role.BigBlind
  
  def isNormal()=
    p.getRole() == Role.Normal


