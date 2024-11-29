package apps.app77
import upickle.default.*
import cs214.webapp.*

type Money = Int

type Pot = Money

type PlayerHand = Set[Card]

type PlayerInfo = (UserId,Money, Role, Status, Option[PlayerHand], BetAmount)

type BetAmount = Money

type Round = Int

enum Suit derives ReadWriter:
  case Heart
  case Diamond
  case Spades
  case Clubs

type CardRepresentation = String

type Card = (Suit, Int, CardRepresentation)

type Deck = List[Card]

enum Status derives ReadWriter:
  case Playing
  case Spectating

enum Role derives ReadWriter:
  case Dealer
  case SmallBlind(amount:Money)
  case BigBlind(amout:Money)
  case Normal

enum Event derives ReadWriter:
  case Check()
  case Fold()
  case Bet(amount:Money)


case class View (
  gameInfo: GameInfo,
  gameConfig: GameConfig
) derives ReadWriter

case class GameConfig(
  maxRound: Round
) derives ReadWriter

case class GameInfo(
  players:List[PlayerInfo],//Ordered list where the 1st item is the current player. Client can be inferred by the Option of hand
  roundNumber : Round,
  communalCards : List[Card],
  pot: Pot,
  logs: List[String],//Logs with last entry being the most recent action
  callAmount: Money,
  minRaise: Money,
  maxRaise:Money,
) derives ReadWriter


enum GamePhase derives ReadWriter:
  case PreFlop
  case Flop
  case Turn
  case River
  case EndRound
  case EndGame


case class State(
  gamePhase: GamePhase,
  gameInfo: GameInfo,
  deck: Deck,
  gameConfig: GameConfig
  ) derives ReadWriter





