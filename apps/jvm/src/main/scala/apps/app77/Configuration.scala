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
   * The initial amount of money you have to pay as a small blind 
  **/
  def getInitialChipIn: Money //LEO refacotr le nom il est guez

  /**
   * How many rounds before we apply to multiply the small blind with getMultiPlyChipIn
  **/
  def getRoundNumberToMultiplyChipIn : Round

  /**
   * The factor with witch the small blind is multiplied every n rounds
  **/
  def getMultiplyChipIn: Int




object Configuration:
  def get(using conf:Configuration)= conf
  
  
