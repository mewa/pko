package pko

import scala.math.BigDecimal

sealed case class CreditState(ongoingMonth: Int, amount: BigDecimal, participationPercentage: BigDecimal = 0) {
  def withParticipationAmount(participationAmount: BigDecimal): CreditState = {
    copy(participationPercentage = participationAmount / amount * 100)
  }
}

object Preconditions {
  def untilMonth(monthInclusive : Int) = (c : CreditState) => c.ongoingMonth <= monthInclusive
  def afterMonth(monthExclusive : Int) = (c : CreditState) => c.ongoingMonth > monthExclusive

  def participationBelow(participationPercentageInclusive : BigDecimal) =
    (c: CreditState) => c.participationPercentage <= participationPercentageInclusive

  def participationAbove(participationPercentageExclusive : BigDecimal) =
    (c: CreditState) => !participationBelow(participationPercentageExclusive)(c)
}
