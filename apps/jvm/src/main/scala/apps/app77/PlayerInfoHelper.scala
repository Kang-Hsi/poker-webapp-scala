package apps.app77

import cs214.webapp.*

/**
 * Some usefull methods for easy manipulation of PlayerInfo type
**/
extension (p :PlayerInfo)

    def withId(id:UserId)=
        (id, p._2, p._3, p._4, p._5, p._6, p._7, p._8)

    def withMoney(money :Money)=
        (p._1, money, p._3, p._4, p._5, p._6, p._7, p._8)

    def withRole(role: Role)=
        (p._1, p._2, role, p._4, p._5, p._6, p._7, p._8)

    def withStatus(status :Status)=
        (p._1, p._2, p._3, status, p._5, p._6, p._7, p._8)

    def withOptionHand(hand: Option[PlayerHand])=
        (p._1, p._2, p._3, p._4, hand, p._6, p._7, p._8)

    def withBetAmount(betAmount: BetAmount)=
        (p._1, p._2, p._3, p._4, p._5, betAmount, p._7, p._8)

    def withHasTalked(hasTalked: Boolean)=
        (p._1, p._2, p._3, p._4, p._5, p._6, hasTalked, p._8)

    def withMoneyBeforeRound(moneyBeforeRound: Money)=
        (p._1, p._2, p._3, p._4, p._5, p._6, p._7, moneyBeforeRound)

    
    def updateMoney(moneyToAddOrSub: Money)=
        p.withMoney(p._2 + moneyToAddOrSub)

    def updateBetAmount(betAmountToAddOrSub : BetAmount)=
        p.withBetAmount(p._6 + betAmountToAddOrSub)

    def fold()=
        p.withStatus(Status.Spectating) //watch out , we might need to reset the betAmount

    def getUserId()=
        p._1

    def getRole()=
        p._3

    def getHand()=
        p._5

    def getStatus()=
        p._4

    def hasTalked()=
        p._7

    def isPlaying()=
        p.getStatus() == Status.Playing || p.getStatus() == Status.AllIn
    
    def isDealer()=
        p.getRole() == Role.Dealer

    def isSmallBlind()=
        p.getRole() == Role.SmallBlind

    def isBigBlind()=
        p.getRole() == Role.BigBlind

    def isNormal()=
        p.getRole() == Role.Normal


