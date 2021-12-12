package me.pbratton.tst

import me.pbratton.tst.BestGroupPricesExercise.getBestGroupPrices
import me.pbratton.tst.PromotionCombinationExercise.allCombinablePromotions
import me.pbratton.tst.PromotionCombinationExercise.combinablePromotions

object Main extends App {
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

  val P1 = Promotion("P1", Seq("P3"))
  val P2 = Promotion("P2", Seq("P4", "P5"))
  val P3 = Promotion("P3", Seq("P1"))
  val P4 = Promotion("P4", Seq("P2"))
  val P5 = Promotion("P5", Seq("P2"))

  println(getBestGroupPrices(Rates, CabinPrices))
  println(allCombinablePromotions(Seq(P1, P2, P3, P4, P5)))
  println(combinablePromotions("P1", Seq(P1, P2, P3, P4, P5)))
  println(combinablePromotions("P3", Seq(P1, P2, P3, P4, P5)))
}
