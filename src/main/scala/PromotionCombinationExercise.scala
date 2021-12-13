package me.pbratton.tst

case class Promotion(code: String, notCombinableWith: Seq[String])

case class PromotionCombo(promotionCodes: Seq[String]) {
  /**
   * Attempts to add a promotion to this promotion combination. If the new promotion is
   * combinable with all promotions in this combination, then the result set contains a single
   * new instance of PromotionCombo with this instance's promotion codes and the newcomer's.
   *
   * If, on the other hand, the new promotion cannot be combined with one or more promotions in this
   * PromotionCombo instance, then result set will contain this instance, and a new PromotionCombo
   * containing the new promotion's code and all of this instance's promotion codes that may
   * be combined with it.
   *
   * Example:
   *
   * val P1 = Promotion("P1", Seq("P3"))
   * val P2 = Promotion("P2", Seq("P4", "P5"))
   * val P3 = Promotion("P3", Seq("P1"))
   * val P4 = Promotion("P4", Seq("P2"))
   * val P5 = Promotion("P5", Seq("P2"))
   *
   * PromotionCombo(Seq("P1")).combine(P2) //=> Seq(PromotionCombo(Seq("P1", "P2")))
   * PromotionCombo(Seq("P1", "P2")).combine(P3) //=> Seq(PromotionCombo(Seq("P1", "P2")), PromotionCombo(Seq("P2", "P3")))
   *
   * @param promotion Promotion to combine
   * @return Seq containing one or more PromotionCodes depending on business logic described above
   */
  def combine(promotion: Promotion): Seq[PromotionCombo] = {
    val (incompatible, compatible) = promotionCodes.partition(c => promotion.notCombinableWith.contains(c))
    if(incompatible.size > 0)
      Seq(this, PromotionCombo(compatible ++ Seq(promotion.code))).filter(_.promotionCodes.size > 0)
    else
      Seq(PromotionCombo(compatible ++ Seq(promotion.code)))
  }

  /**
   * Returns true if the other PromotionCombo's promotion codes are completely contained within this
   * instance's promotion codes.
   *
   * Example:
   *
   * PromotionCombo(Seq("P1")).isSuperSetOf(PromotionCombo(Seq("P4", "P5"))) //=> false
   * PromotionCombo(Seq("P1", "P4", "P5")).isSuperSetOf(PromotionCombo(Seq("P4", "P5"))) //=> true
   *
   * @param other
   * @return
   */
  def isSuperSetOf(other: PromotionCombo): Boolean =
    promotionCodes.size > other.promotionCodes.size &&
      other.promotionCodes.forall(promotionCodes.contains(_))
}

object PromotionCombo {
  def apply(promotion: Promotion): PromotionCombo = PromotionCombo(Seq(promotion.code))
}

object PromotionCombinationExercise {
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    // Moving left to right, begin a new combination for each promotion, and then attempt to
    // combine it with combinations created from earlier promotions in the parameter set.
    val allPromotionCombinationsWithSubsets =
      allPromotions.foldLeft[Set[PromotionCombo]](Set()) { case (combinations, nextPromotion) =>
        combinations.flatMap(_.combine(nextPromotion)) ++ Set(PromotionCombo(nextPromotion))
      }

    // We are only interested in maximal combinations, so filter out any combinations that are
    // wholely contained elsewhere in the result set.
    allPromotionCombinationsWithSubsets
      .filterNot { possibleSubset =>
        allPromotionCombinationsWithSubsets.exists(_.isSuperSetOf(possibleSubset))
      }.toSeq
  }

  def combinablePromotions(
    promotionCode: String,
    allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] =
    for {
      // Compute all possible combinations
      allCombinable <- allCombinablePromotions(allPromotions)
      // Return only combinations that contain the promotionCode passed in as a parameter.
      if allCombinable.promotionCodes.contains(promotionCode)
    } yield {
      allCombinable
    }
}
