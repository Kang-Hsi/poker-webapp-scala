package apps.app77

import cs214.webapp.*
import scala.util.{Failure, Success, Try}
import ujson.Value


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
    override def encode(view: View): Value = ujson.Arr(
      viewFormatHelpers.encodeGameInfo(view.gameInfo),
      viewFormatHelpers.encodeGameConfig(view.gameConfig),
      viewFormatHelpers.encodeGamePhase(view.gamePhase)
      )
    override def decode(json: Value): Try[View] = Try{
      val arr = json.arr
      val gameInfo = viewFormatHelpers.decodeGameInfo(arr(0)).get
      val gameConfig = viewFormatHelpers.decodeGameConfig(arr(1)).get
      val gamePhase = viewFormatHelpers.decodeGamePhase(arr(2)).get
      View(gameInfo, gameConfig, gamePhase)
    }

