package me.pbratton.tst

import org.scalatest.flatspec.AnyFlatSpec

class BestGroupPricesExerciseSpec extends AnyFlatSpec {
  val Rates = Seq(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior")
  )

  val CabinPrices = Seq(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00)
  )

  it should "return Nil if prices are Nil" in {
    assertResult(Nil)(BestGroupPricesExercise.getBestGroupPrices(Nil, Nil))
    assertResult(Nil)(BestGroupPricesExercise.getBestGroupPrices(Rates, Nil))
  }

  it should "return Nil if rates are Nil" in {
    assertResult(Nil)(BestGroupPricesExercise.getBestGroupPrices(Nil, CabinPrices))
  }

  it should "return best group prices per cabin and rate group" in {
    assertResult(Seq(
      BestGroupPrice("CB", "S1", 245.00, "Senior"),
      BestGroupPrice("CA", "S1", 225.00, "Senior"),
      BestGroupPrice("CB", "M1", 230.00, "Military"),
      BestGroupPrice("CA", "M1", 200.00, "Military")
    ))(BestGroupPricesExercise.getBestGroupPrices(Rates, CabinPrices))
  }
}
