package me.pbratton.tst

case class Rate(rateCode: String, rateGroup: String)

case class CabinPrice(
  cabinCode: String,
  rateCode: String,
  price: BigDecimal
)

case class BestGroupPrice(
  cabinCode: String,
  rateCode: String,
  price: BigDecimal,
  rateGroup: String
)

object BestGroupPricesExercise {
  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    // First we will hydrate each CabinPrice with its corresponding rateGroup
    val groupPrices = for {
      price <- prices
      rateForPrice <- rates.find(_.rateCode == price.rateCode)
    } yield {
      BestGroupPrice(price.cabinCode, price.rateCode, price.price, rateForPrice.rateGroup)
    }

    // Group best prices by cabin and rate group
    val bestGroupPriceByCabinCodeAndRateGroup = groupPrices.groupBy(x => (x.cabinCode, x.rateGroup)).values

    // Take the lowest price in each grouping
    bestGroupPriceByCabinCodeAndRateGroup.map(_.minBy(_.price)).toSeq
  }
}
