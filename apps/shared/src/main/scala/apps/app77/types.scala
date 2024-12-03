package apps.app77
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

type potContribution = Money

/* Complete nformation of a player */
type PlayerInfo = (UserId, Money, Role,
                  Status, Option[PlayerHand],
                  BetAmount, hasTalked, potContribution
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




