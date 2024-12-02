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


    /* enum Event derives ReadWriter:
  case Check()
  case Fold()
  case Bet(amount:Money) */

