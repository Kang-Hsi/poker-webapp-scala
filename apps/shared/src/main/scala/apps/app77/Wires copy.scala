package apps.app77

import cs214.webapp.*
import scala.util.{Failure, Success, Try}
import ujson.Value
import upickle.default.*


object WireCopy extends AppWire[Event, View]:
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

    def encodeSuit(suit : Suit) : Value = suit match
      case Suit.Heart => ujson.Str("heart")
      case Suit.Diamond => ujson.Str("diamond")
      case Suit.Spades => ujson.Str("spades")
      case Suit.Clubs => ujson.Str("clubs")

    def decodeSuit(json : Value) : Try[Suit] = Try { json.str match
      case "heart" => Suit.Heart
      case "diamond" => Suit.Diamond
      case "spades" => Suit.Spades
      case "clubs" => Suit.Clubs
      case _ => throw new IllegalArgumentException("Unknown suit")
    }

    def encodeCard(card : Card) : Value = ujson.Arr(encodeSuit(card._1), ujson.Num(card._2), ujson.Str(card._3))
    def decodeCard(ujson : Value) : Try[Card] = Try {
      val array = ujson.arr
      val suit = decodeSuit(array(0)).get
      val value = array(1).num.toInt
      val representation = array(2).str
      new Card(suit, value, representation)
    }
    def encodeHand(hand : PlayerHand) : Value = ujson.Arr(hand.map(card => encodeCard(card)))
    def decodeHand(ujson : Value) : Try[PlayerHand] = Try {
      val array = ujson.arr
      (array.map(ujson => decodeCard(ujson).get).toSet)
    }

    def encodeStatus(status : Status) : Value = status match
      case Status.Playing => ujson.Str("playing")
      case Status.Spectating => ujson.Str("spectating")
    def decodeStatus(ujson : Value) : Try[Status] = Try { ujson.str match
      case "playing" => Status.Playing
      case "spectating" => Status.Spectating
      case _ => throw new IllegalArgumentException("Unknown status")
    }

    def encodeRole(role : Role) : Value = role match
      case Role.Dealer => ujson.Str("dealer")
      case Role.SmallBlind => ujson.Str("smallBlind")
      case Role.BigBlind => ujson.Str("bigBlind")
      case Role.Normal => ujson.Str("normal")

    def decodeRole(ujson : Value) : Try[Role] =  Try { ujson.str match
      case "dealer" => Role.Dealer
      case "smallBlind" => Role.SmallBlind
      case "bigBlind" => Role.BigBlind
      case "normal" => Role.Normal
      case _ => throw new IllegalArgumentException("Unknown role")
    }

    def encodeOption[T](option : Option[T])(encode : T => Value) : Value = option match
      case None => ujson.Obj("type" -> "none")
      case Some(value) => ujson.Obj("type" -> "some", "value" -> encode(value))



    def encodePlayerInfo(playerInfo : PlayerInfo) : Value = ujson.Arr(
      ujson.Str(playerInfo._1), ujson.Num(playerInfo._2), encodeRole(playerInfo._3),
      encodeStatus(playerInfo._4), playerInfo._5.encodeOption(hand => encodeHand(hand)),ujson.Num(playerInfo._6), ujson.Bool(playerInfo._7), ujson.Num(playerInfo._8))

      def decodePlayerInfo(ujson: Value): Try[PlayerInfo] = Try {
        val arr = ujson.arr
        val userId = arr(0).str
        val money = arr(1).num.toInt
        val role = decodeRole(arr(2)).get
        val status = decodeStatus(arr(3)).get
        val playerHand = arr(4).decodeOption(decodeHand).get
        val betAmount = arr(5).num.toInt
        val hasTalked = arr(6).bool
        val moneyBeforeRound = arr(7).num.toInt
        new PlayerInfo(userId, money, role, status, playerHand, betAmount, hasTalked, moneyBeforeRound)
    }

    extension(ujson: Value)
      def decodeOption[T](decode : Value => Try[T]) : Try[Option[T]] = Try {
        val obj = ujson.obj
        obj("type").str match
          case "none" => None
          case "some" => Some(decode(obj("value")).get)
      }

    extension[T](option : Option[T])
      def encodeOption[T](encode : T => Value) : Value = option match
        case None => ujson.Obj("type" -> "none")
        case Some(value) => ujson.Obj("type" -> "some", "value" -> encode(value))

    def encodeGameConfig(gameConfig : GameConfig) : Value = ???
    def encodeGameInfo(gameInfo : GameInfo) : Value = ???
    def encodeState(state : State) : Value = ???
    def encodeGamePhase(gamePhase : GamePhase) : Value = ???

    override def encode(view: View): Value = upickle.default.write(view)
    override def decode(json: Value): Try[View] = Try(upickle.default.read[View](json))


    /* enum Event derives ReadWriter:
  case Check()
  case Fold()
  case Bet(amount:Money) */

