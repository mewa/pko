package pko.main

import pko._
import pko.Preconditions._

object Main extends App {
  implicit def strToBigDecimal = (s: String) => BigDecimal(s)

  val state = CreditState(ongoingMonth = 13, amount = 75000)
    .withParticipationAmount(20000)

  val offers = List(
    MarginOffer("1.1", untilMonth(1)),
    MarginOffer(2.222, untilMonth(13)),
    MarginOffer(3, untilMonth(13), amountBelow(75000)),
    MarginOffer(4, untilMonth(13), amountBelow(75001)),
    MarginOffer(5, untilMonth(13), amountBelow(75001)),
    MarginOffer(6, untilMonth(13), amountAbove(75000)),
    MarginOffer("7", untilMonth(13), amountAbove(75001)),
    MarginOffer(8, untilMonth(13), amountAbove(10000),
      participationAbove(10)),
    MarginOffer(9, untilMonth(13), amountAbove(10000),
      participationAbove(30)),
  )

  for (offer <- offers) {
    if (offer.isValidFor(state)) {
      printf("Offer with margin %s applies\n", offer.margin)
    } else {
      printf("Offer with margin %s does not apply\n", offer.margin)
    }
  }

  printf("Valid offers: %d\n", offers.count(_.isValidFor(state)))
  val validOffers = offers.filter(_.isValidFor(state))
  if (validOffers.length > 0) {
    printf("Lowest applicable margin: %s\n", validOffers.minBy(_.margin))
  }



  // example offers generated programatically
  val limits = {
    val limits = List(Some(40000), Some(80000), Some(120000), Some(200000), Some(700000), None)
      (None +: limits).zip(limits).map(pair => {
        pair match {
          case (Some(start), Some(end)) => List(amountAbove(start), amountBelow(end))
          case (None, Some(end)) => List(amountBelow(end))
          case (Some(start), None) => List(amountAbove(start))
          case _ => List()
        }
      })
  }

  val participations = {
    val participation = List(10, 20, 30, 50, 100)
    participation.zip(participation.tail)
      .map{
        case (start, end) => List(participationAbove(start))
      }
  }

  val examplePreconditions = for (
    limit <- limits;
    participation <- participations
  ) yield afterMonth(12) +: List(limit, participation).flatten

  // example offers with specified preconditions
  // exact margin values omitted for simplicity
  val exampleMarginOffers = MarginOffer(1, untilMonth(12)) +: examplePreconditions.zipWithIndex.map{
    case (preconditions, index) => MarginOffer(index * 10, preconditions : _*)
  }
}
