package apps.app77

object Logger:
  def debug(msg:String)(using conf: Configuration)=
    if conf.loggerPrintLevel ==3 then
      println("DEBUG : " + msg)
    
  def info(msg:String)(using conf: Configuration)=
    if conf.loggerPrintLevel >= 2 then
      println("INFO : " + msg)

  def warn(msg:String)(using conf: Configuration)=
    if conf.loggerPrintLevel >=1 then
      println("WARN : " + msg)
     
