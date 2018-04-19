package pko

import org.scalatest._

class CreditStateSpec extends FlatSpec with Matchers {
  "withParticipationAmount" should "should calculate correct percentage" in {
    val amount = 10000
    val participation = 2500

    val state = CreditState(ongoingMonth = 1, amount = amount)
      .withParticipationAmount(participation)

    state.participationPercentage should be (25)
  }

}
