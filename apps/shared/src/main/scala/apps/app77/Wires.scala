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
    override def encode(event: Event): Value = upickle.default.write(event)
    override def decode(json: Value): Try[Event] = Try(upickle.default.read[Event](json))

  override object viewFormat extends WireFormat[View] :
    override def encode(view: View): Value = upickle.default.write(view)
    override def decode(json: Value): Try[View] = Try(upickle.default.read[View](json))


