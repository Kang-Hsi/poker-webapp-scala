package apps.app77

/** A configuration trait that groupes all the informations we might need for a
  * poker game.
  */
trait Configuration:

  /* The number of rounds before the end of a game. */
  def getMaxRound: Round

  /* The initial money of each player at the beginning of a game. */
  def getInitialMoney: Money

  /* Get small blind.*/
  def getSmallBlind: Money

  /** Get big blind.
    *
    * @return
    *   big blind amount
    */
  def getBigBlind: Money

  /** How many rounds before we apply to multiply the small blind with
    * getMultiPlyChipIn.
    *
    * Not used but could be useful for future extensions.
    */
  def getRoundNumberToMultiplyChipIn: Round

  /** Returs the log level for the stdout : 3 = print everything (DEBUGS, INFO &
    * WARN) 2 = print only INFO & WARN 1 = print onyl WARN 0 = print nothing
    */
  def loggerPrintLevel: Int

object Configuration:
  given standardConfig: Configuration with
    override def getMaxRound: Round = 20
    override def getInitialMoney: Money = 100
    override def getSmallBlind: Money = 1
    override def getBigBlind: Money = 2
    override def getRoundNumberToMultiplyChipIn: Round = 5
    override def loggerPrintLevel = 3

  /** Returns configuration.
    *
    * @param conf
    *   the configuration.
    * @return
    *   configuration.
    */
  def get(using conf: Configuration) = conf
