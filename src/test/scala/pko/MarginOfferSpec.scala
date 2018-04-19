package pko

import org.scalatest._

class MarginOfferSpec extends FlatSpec with Matchers {
  "MarginOffer" should "be valid" in {
    val state = CreditState(ongoingMonth = 1, amount = 10000, participationPercentage = 50)

    val offer = MarginOffer(123,
      Preconditions.untilMonth(1),
      Preconditions.participationAbove(49),
      Preconditions.amountAbove(10000))

    offer.isValidFor(state) should be (true)
  }

  "MarginOffer" should "be invalid" in {
    val state = CreditState(ongoingMonth = 1, amount = 10000, participationPercentage = 50)

    {
      val offer = MarginOffer(123,
        Preconditions.untilMonth(1),
        Preconditions.participationAbove(50),
        Preconditions.amountAbove(10000))

      offer.isValidFor(state) should be (false)
    }
    {
      val offer = MarginOffer(123,
        Preconditions.afterMonth(10),
        Preconditions.participationAbove(50),
        Preconditions.amountAbove(100050))

      offer.isValidFor(state) should be (false)
    }
  }
}
