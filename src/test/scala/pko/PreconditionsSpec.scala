package pko

import org.scalatest._

class PreconditionsSpec extends FlatSpec with Matchers {
  "Preconditions.untilMonth" should "be true for months before, end-month inclusive" in {
    val state = CreditState(ongoingMonth = 1, amount = 10000)

    Preconditions.untilMonth(12)(state) should be (true)
    Preconditions.untilMonth(12)(state.copy(ongoingMonth = 1)) should be (true)
  }

  it should "be false for months after" in {
    val state = CreditState(ongoingMonth = 13, amount = 10000)

    Preconditions.untilMonth(12)(state) should be (false)
  }

  "Preconditions.afterMonth" should "be false for earlier months, end-month exclusive" in {
    val state = CreditState(ongoingMonth = 12, amount = 10000)

    Preconditions.afterMonth(12)(state) should be (false)
    Preconditions.afterMonth(12)(state.copy(ongoingMonth = 1)) should be (false)
  }

  it should "be valid for following months" in {
    val state = CreditState(ongoingMonth = 13, amount = 10000)

    Preconditions.afterMonth(12)(state) should be (true)
  }

  "Preconditions.participationBelow" should "be true for lower values, end-value inclusive" in {
    val state = CreditState(ongoingMonth = 12, amount = 10000, participationPercentage = 0)

    Preconditions.participationBelow(25)(state) should be (true)
    Preconditions.participationBelow(25)(state.copy(participationPercentage = 25)) should be (true)
  }

  it should "be false for higher values" in {
    val state = CreditState(ongoingMonth = 12, amount = 10000, participationPercentage = 26)

    Preconditions.participationBelow(25)(state) should be (false)
  }

  "Preconditions.participationAbove" should "complement .participationBelow" in {
    for (participation <- Range(10, 40, 5)) {
      val state = CreditState(ongoingMonth = 12, amount = 10000, participationPercentage = 25)

      Preconditions.participationAbove(participation)(state) should be
        (!Preconditions.participationBelow(participation)(state))
    }
  }
}
