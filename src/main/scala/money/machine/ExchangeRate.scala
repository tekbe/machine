package money.machine

import Akka._
import Combine._
import Time._

import scala.collection._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

// @form.atter:off
/**
  * compare asks and bids of different exchanges
  */
object ExchangeRate {

  type SOME_EXCHANGE = Exchange[E] forSome {type E <: Exchange[E]}
  type SOME_EXCHANGERATE[C<:Coin, CC<:Currency] = ExchangeRate[E1, E2, C, CC] forSome {type E1 <: Exchange[E1]; type E2 <: Exchange[E2]}
  type SOME_ORDERBOOK[C<:Coin, CC<:Currency] = OrderBook[E, C, CC] forSome {type E <: Exchange[E]}

  def max[C <: Coin, CC <: Currency](er1: SOME_EXCHANGERATE[C,CC],
                                     er2: SOME_EXCHANGERATE[C,CC],
                                     minRatePercent: Percent,
                                     minPriceDifference: Price[CC],
                                     coinVolumeRange: CoinVolumeRange[C,CC]): SOME_EXCHANGERATE[C,CC] =

    (er1.actualRate(minRatePercent, minPriceDifference, coinVolumeRange), er2.actualRate(minRatePercent, minPriceDifference, coinVolumeRange)) match {
      case (Some(_), None) => er1
      case (None, Some(_)) => er2
      case (Some(rate1), (Some(rate2))) => if (rate1 >= rate2) er1 else er2
      case (None, None) => if (er1.highestPossibleRate >= er2.highestPossibleRate) er1 else er2
    }

  /** stream of highest exchange rates between given exchanges */
  def stream[C <: Coin, CC <: Currency](exchanges: SOME_EXCHANGE*)
                                       (coin: C,
                                        currency: CC,
                                        minRatePercent: Percent,
                                        minPriceDifference: Price[CC],
                                        coinVolumeRange: CoinVolumeRange[C,CC]): Stream[SOME_EXCHANGERATE[C, CC]] = {

    def exchangeRateStream: Stream[Future[Option[SOME_EXCHANGERATE[C, CC]]]] = {

      // fetch order books from exchanges
      val orderBooks: Future[Seq[SOME_ORDERBOOK[C, CC]]] = combineFutures(exchanges.map(_.orderBook(coin, currency)))

      val maxRate = orderBooks.map { orderBooks =>

        // create all possible exchange rates
        val exchangeRates = for {
          ob1 <- orderBooks
          ob2 <- orderBooks
          if ob1.exchange != ob2.exchange && ob1.sameTime(ob2)
        } yield ExchangeRate(ob1.asks, ob2.bids).asInstanceOf[SOME_EXCHANGERATE[C, CC]]

        // find best exchange rate
        exchangeRates.foldLeft(exchangeRates.headOption)((er1Option, er2) => er1Option match {
          case None => Some(er2)
          case Some(er1) => Some(max[C, CC](er1, er2, minRatePercent, minPriceDifference, coinVolumeRange))
        })
      }

      maxRate #:: exchangeRateStream
    }

    // as blocking stream
    exchangeRateStream
      .map(f => Try(Await.result(f, coin.latencyTolerance)))
      .map {
        case Success(erOpt) => erOpt
        case Failure(e) =>
          println(s"exception (exchange rate stream): ${e.getMessage}")
          Thread.sleep(5000) // ...
          None
      }.collect { case Some(er) => er }
  }
}

/** exchange rate in percent when matching bids and asks from different exchanges. */
case class ExchangeRate[E1 <: Exchange[E1], E2 <: Exchange[E2], C <: Coin, CC <: Currency](askBook: AskBook[E1, C, CC], bidBook: BidBook[E2, C, CC]) {

  assert(!askBook.isEmpty)
  assert(!bidBook.isEmpty)
  assert(askBook.sameTime(bidBook))

  /** returns price rates for which matching asks and bids exist */
  def matchingPriceRates: immutable.Seq[PriceRate[C, CC]] =
    (bidBook.offers.map(_.priceRate).filter(_ >= askBook.head.priceRate) ++
      askBook.offers.map(_.priceRate).filter(_ <= bidBook.head.priceRate)).distinct.sorted.reverse

  /** actual rate when turned into orders */
  def actualRate(minRatePercent: Percent, minPriceDifference: Price[CC], coinVolumeRange: CoinVolumeRange[C,CC]): Option[Percent] =
    orders(minRatePercent, minPriceDifference, coinVolumeRange).map { o =>
      Math.ratePercent(bidBook.price(o._2.volume.coinVolume), askBook.price(o._1.volume.coinVolume))
    }

  def actualPriceDifference(minRatePercent: Percent, minPriceDifference: Price[CC], coinVolumeRange: CoinVolumeRange[C,CC]): Option[Price[CC]] =
    orders(minRatePercent, minPriceDifference, coinVolumeRange).map { o =>
      bidBook.price(o._2.volume.coinVolume) - askBook.price(o._1.volume.coinVolume)
    }

  /** max rate between lowest ask and highest bid (might be negative) */
  def highestPossibleRate: Percent = Math.ratePercent(bidBook.head.priceRate, askBook.head.priceRate)

  /** turns matching asks and bids into orders (respecting the given limits).
    * min rate < 0% has no effect. */
  def orders(minRatePercent: Percent, minPriceDifference: Price[CC], coinVolumeRange: CoinVolumeRange[C,CC]): Option[(BuyOrder[E1, C, CC], SellOrder[E2, C, CC])] = {
    // valid price rates with required volume
    matchingPriceRates.filter(rate => askBook.coinVolume(rate) >= coinVolumeRange.min.coinVolume && bidBook.coinVolume(rate) >= coinVolumeRange.min.coinVolume)
      // valid volumes
      .map(rate => Math.min(coinVolumeRange.max.coinVolume, Math.min(askBook.coinVolume(rate), bidBook.coinVolume(rate))))
      .distinct
      // volumes and price exchange rate percent
      .map(volume => (volume, Math.ratePercent(bidBook.price(volume), askBook.price(volume))))
      // rate above limit (e.g. higher than transaction costs on exchanges)
      .filter(_._2 >= minRatePercent)
      // volumes with difference between bids and asks prices for given volume
      .map(v => (v._1, bidBook.price(v._1) - askBook.price(v._1)))
      // filter by price difference limit
      .filter(_._2 >= minPriceDifference)
      // max out by price difference
      .sortBy(_._2).reverse
      // select best volume (if any)
      .map(_._1)
      // to orders
      .headOption.map(volume => (askBook.createOrder(volume), bidBook.createOrder(volume)))
  }

  def from: Exchange[E1] = askBook.exchange

  def to: Exchange[E2] = bidBook.exchange

  def coin: C = askBook.coin

  def currency: CC = askBook.currency
}
