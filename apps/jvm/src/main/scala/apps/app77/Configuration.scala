package apps.app77


/**
 * A configuration trait that groupes all the informations we might
 * need for a poker game.
 **/
trait Configuration:

  /**
   * The number of rounds before the end of a game
  **/
  def getMaxRound: Round

  /**
   * The initial money of each player at the beginning of a game
  **/
  def getInitialMoney: Money

  /**
   * Get small blind.
  **/
  def getSmallBlind: Money 

  /**
    * Get big blind.
    *
    * @return big blind amount
    */
  def getBigBlind: Money

  /**
   * How many rounds before we apply to multiply the small blind with getMultiPlyChipIn
  **/
  def getRoundNumberToMultiplyChipIn : Round

  /**
   * Returs the log level for the stdout :
   * 3 = print everything (DEBUGS, INFO & WARN)
   * 2 = print only INFO & WARN
   * 1 = print onyl WARN
   * 0 = print nothing
  **/
  def loggerPrintLevel: Int

  


object Configuration:
  given standardConfig: Configuration with
    def getMaxRound: Round = 20
    def getInitialMoney: Money = 100
    def getSmallBlind: Money = 1
    def getBigBlind: Money = 2
    def getRoundNumberToMultiplyChipIn: Round = 5
    def loggerPrintLevel = 3
    
  def get(using conf:Configuration)= conf


