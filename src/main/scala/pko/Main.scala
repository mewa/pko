package pko

import scala.math.BigDecimal

sealed case class CreditState(ongoingMonth: Int, amount: BigDecimal, participation: BigDecimal = 0) {
  def withParticipationPercentage(percentage: BigDecimal) = {
    val percentageDecimal = percentage / 100
    copy(participation = amount * percentageDecimal)
  }
}

object Preconditions {
  def untilMonth(monthInclusive : Int) = (c : CreditState) => c.ongoingMonth <= monthInclusive
  def afterMonth(monthExclusive : Int) = (c : CreditState) => c.ongoingMonth > monthExclusive
}
