package apps.app77

object Logger:
  /** Prints debug message.
    *
    * @param msg
    *   a message
    * @param conf
    *   game configuration.
    */
  def debug(msg: String)(using conf: Configuration) =
    if conf.loggerPrintLevel == 3 then println("DEBUG : " + msg)

  /** Prints info message.
    *
    * @param msg
    *   a message
    * @param conf
    *   game configuration
    */
  def info(msg: String)(using conf: Configuration) =
    if conf.loggerPrintLevel >= 2 then println("INFO : " + msg)

  /** Prints warning message.
    *
    * @param msg
    *   a message.
    * @param conf
    *   game configuration.
    */
  def warn(msg: String)(using conf: Configuration) =
    if conf.loggerPrintLevel >= 1 then println("WARN : " + msg)
