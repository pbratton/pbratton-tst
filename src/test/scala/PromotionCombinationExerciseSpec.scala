package me.pbratton.tst

import org.scalatest.flatspec.AnyFlatSpec

class PromotionCombinationExerciseSpec extends AnyFlatSpec {
  val P1 = Promotion("P1", Seq("P3"))
  val P2 = Promotion("P2", Seq("P4", "P5"))
  val P3 = Promotion("P3", Seq("P1"))
  val P4 = Promotion("P4", Seq("P2"))
  val P5 = Promotion("P5", Seq("P2"))

 "allCombinablePromotions" should "returns Nil if no promotions provided" in {
    assertResult(Nil)(PromotionCombinationExercise.allCombinablePromotions(Nil))
  }

  "allCombinablePromotions" should "removes duplicates from result set" in {
    val combinations = PromotionCombinationExercise.allCombinablePromotions(Seq(P1, P2, P3))
    assert(combinations.size == combinations.distinct.size)
  }

  "allCombinablePromotions" should "respects non combination business rules" in {
    val promotionTestSet = Seq(P1, P2, P3)
    val combinations = PromotionCombinationExercise.allCombinablePromotions(Seq(P1, P2, P3))
    promotionTestSet.foreach { promotion =>
      assert(combinations.contains(PromotionCombo(Seq(P1.code, P2.code))))
      assert(combinations.contains(PromotionCombo(Seq(P2.code, P3.code))))
      assert(!combinations.contains(PromotionCombo(Seq(P1.code, P3.code))))
    }
  }
}
