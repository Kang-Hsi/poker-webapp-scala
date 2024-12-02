package apps.app77

import cs214.webapp.*
import scala.util.{Failure, Success, Try}
import ujson.Value
import upickle.default.*


object Wire extends AppWire[Event, View]:
  import Event.*
  import View.*
  import ujson.*

  override object eventFormat extends WireFormat[Event] :
    override def encode(event: Event): Value = event match

      case Check() => ujson.Obj("type" -> "check")
      case Fold()  => ujson.Obj("type" -> "fold")
      case Bet(amount) => ujson.Obj("type" -> "bet", "amount" -> ujson.Num(amount))

    override def decode(json: Value): Try[Event] = Try {
      val obj = json.obj
      obj("type").str match
        case "check" => Event.Check()
        case "fold"  => Event.Fold()
        case "bet"   =>
          val amount = obj("amount").num.toInt
          Event.Bet(amount)
    }

  override object viewFormat extends WireFormat[View] :
    override def encode(view: View): Value = upickle.default.write(view)
    override def decode(json: Value): Try[View] = Try(upickle.default.read[View](json))

    def encodeSuit(suit : Suit) : Value = ???
    def encodeCard(card : Card) : Value = ???
    def encodeHand(hand : PlayerHand) : Value = ???
    def encodeStatus(status : Status) : Value = ???
    def encodeRole(role : Role) : Value = ???
    def encodePlayerInfo(playerInfo : PlayerInfo) : Value = ???
    def encodeGameConfig(gameConfig : GameConfig) : Value = ???
    def encodeGameInfo(gameInfo : GameInfo) : Value = ???
    def encodeState(state : State) : Value = ???
    def encodeGamePhase(gamePhase : GamePhase) : Value = ???


   /*
   package apps.app77
import upickle.default.*
import cs214.webapp.*

/* Defining the cards.
 *
 * The four suits.
 */
enum Suit:
  case Heart
  case Diamond
  case Spades
  case Clubs

/* Card representation as a string.  */
type CardRepresentation = String

/* A (poker) card */
type Card = (Suit, Int, CardRepresentation)

/* A deck of cards */
type Deck = List[Card]

/* Money in the game (only integers) */
type Money = Int

/* Pot of a poker round */
type Pot = Money

/* The hand (cards) of a player */
type PlayerHand = Set[Card]

/* Bet amount */
type BetAmount = Money

/* Rounds of a complete game of poker */
type Round = Int

/* The status of a player, playing or spectating */
enum Status:
  case Playing
  case AllIn
  case Spectating

/* The role of a player */
enum Role:
  case Dealer
  case SmallBlind
  case BigBlind
  case Normal

type hasTalked = Boolean

type moneyBeforeRound = Money

/* Complete nformation of a player */
type PlayerInfo = (UserId, Money, Role,
                  Status, Option[PlayerHand],
                  BetAmount, hasTalked, moneyBeforeRound
                  )


/* The events a player can trigger */
enum Event:
  case Check()
  case Fold()
  case Bet(amount:Money) //used also for Call !!!!!!!


case class GameConfig(
  maxRound: Round,
  smallBlind: Int,
  bigBlind: Int
)

case class View (
  gameInfo: GameInfo,
  gameConfig: GameConfig
)

/**
  * The configuration/information of our poker game.
  *
  * @param players the list of players
  * @param roundNumber the number of rounds the game will last
  * @param communalCards the communal cards (cards in the middle)
  * @param pot the pot (size)
  * @param logs the log of every action
  * @param callAmount the call amount
  * @param minRaise the minimum raise
  * @param maxRaise the maximum raise
  */
case class GameInfo(
  players:List[PlayerInfo],//Ordered list where the 1st item is the current player. Client can be inferred by the Option of hand
  roundNumber : Round,
  communalCards : List[Card],
  pot: Pot,
  logs: List[String],//Logs with last entry being the most recent action
  callAmount: Money,
  minRaise: Money,
  maxRaise: Money,
)


case class State(
  gamePhase: GamePhase,
  gameInfo: GameInfo,
  deck: Deck,
  gameConfig: GameConfig
  )

enum GamePhase:
  case PreFlop
  case Flop
  case Turn
  case River
  case EndRound
  case EndGame
 */

