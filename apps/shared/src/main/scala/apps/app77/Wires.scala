package apps.app77

import cs214.webapp.*
import scala.util.{Failure, Success, Try}
import ujson.Value


object Wire extends AppWire[Event, View]:
  import Event.*
  import View.*
  import ujson.*

  /**
    * Object provided to encode and decode an event.
    */
  override object eventFormat extends WireFormat[Event] :

    /**
      * Provides a JSON encoding of an Event.
      * Converts an Event instance into a JSON object that represents the event type and its associated data.
      *
      * @param event the Event instance to encode
      * @return the JSON representation of the event
      */
    override def encode(event: Event): Value = event match

      case Check() => ujson.Obj("type" -> "check")
      case Fold()  => ujson.Obj("type" -> "fold")
      case Bet(amount) => ujson.Obj("type" -> "bet", "amount" -> ujson.Num(amount))

    /**
      * Decodes a JSON object into an Event instance.
      * Attempts to convert a JSON object into an Event
      * by extracting the type of the event and any necessary data.
      *
      * @param json the JSON value of the event
      * @return A Try instance encapsulating the View, either Success with the View
      * or Failure with an exception if any part fails to decode correctly.
      */
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

    /**
      * Encodes a View into a JSON object.
      * Consolidates various aspects of the game view into a JSON array
      * containing game information, configuration, and phase.
      *
      * @param view the View instance to encode
      * @return the JSON representation of the view
      */
    override def encode(view: View): Value = ujson.Arr(
      viewFormatHelpers.encodeGameInfo(view.gameInfo),
      viewFormatHelpers.encodeGameConfig(view.gameConfig),
      viewFormatHelpers.encodeGamePhase(view.gamePhase)
      )
    /**
      * Decodes a JSON array into a View instance.
      * Extracts game information, configuration, and phase from a JSON array
      * and combines them into a View.
      *
      * @param json the JSON array representing the view
      * @return A Try instance encapsulating the View, either Success with the View
      * or Failure with an exception if any part fails to decode correctly.
      */
    override def decode(json: Value): Try[View] = Try{
      val arr = json.arr
      val gameInfo = viewFormatHelpers.decodeGameInfo(arr(0)).get
      val gameConfig = viewFormatHelpers.decodeGameConfig(arr(1)).get
      val gamePhase = viewFormatHelpers.decodeGamePhase(arr(2)).get
      View(gameInfo, gameConfig, gamePhase)
    }

