package money.machine

import java.time.LocalTime

import money.machine.Akka._
import money.machine.Akka.actorSystem.scheduler
import money.machine.ExchangeBalances.sufficientBalance
import money.machine.ExchangeRate.{SOME_EXCHANGE, SOME_EXCHANGERATE}

import scala.concurrent.duration.DurationDouble
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}

// @fo.rmatter:off
object MoneyMachine {

  // wait for price to drop below limit, then buy as much as possible
  def buyBelowLimit[E <: Exchange[E], C <: Coin, CC <: Currency](e: Exchange[E], limit: PriceRate[C,CC]): Unit = {
    ExchangeBalances.withExchanges(e)

    def go(): Unit = {

      e.orderBook(limit.coin, limit.currency).onComplete{
        case Success(v) => buy(v.asks); scheduler.scheduleOnce(delay = 10 seconds)(go())
        case Failure(t) => println(s"failed to fetch order book: ${t.getMessage}"); scheduler.scheduleOnce(delay = 10 seconds)(go())
      }
    }

    def buy(asks: AskBook[E,C,CC]): Unit = {
      println(s"rate: ${asks.head.priceRate} -- buy limit: $limit")
      if (asks.offers.head.priceRate < limit) {
        val volume = asks.coinVolume(limit)
        val order = asks.createOrder(volume)
        if (sufficientBalance(order)) {
          e.place(asks.createOrder(volume), true)
          println(s"placed order $order")
        } else {
          println(s"insufficient balance for order $order")
        }
      }
    }

    go()
  }

  // wait for price go above limit, then sell as much as possible
  def sellAboveLimit[E <: Exchange[E], C <: Coin, CC <: Currency](e: Exchange[E], limit: PriceRate[C,CC]): Unit = {
    ExchangeBalances.withExchanges(e)

    def go(): Unit = {

      e.orderBook(limit.coin, limit.currency).onComplete{
        case Success(v) => sell(v.bids); scheduler.scheduleOnce(delay = 10 seconds)(go())
        case Failure(t) => println(s"failed to fetch order book: ${t.getMessage}"); scheduler.scheduleOnce(delay = 10 seconds)(go())
      }
    }

    def sell(bids: BidBook[E,C,CC]): Unit = {
      println(s"rate: ${bids.head.priceRate} -- sell limit: $limit")
      if (bids.offers.head.priceRate > limit) {
        val volume = bids.coinVolume(limit)
        val order = bids.createOrder(volume)
        if (sufficientBalance(order)) {
          e.place(bids.createOrder(volume), true)
          println(s"placed order $order")
        } else {
          println(s"insufficient balance for order $order")
        }
      }
    }

    go()
  }


  def averagePriceRate[CC <: Currency, C <: Coin](c: C, cc: CC)(es: Seq[Exchange[_]] = Exchange.exchanges) = {
    val priceRates = Await.result(Combine.combineFutures(es.map(_.priceRate(c,cc))), 10 seconds)
    PriceRate(c, priceRates.foldLeft(BigDecimal(0))((acc, rate) => acc + rate.rate) / es.size, cc)
  }

  def minPriceDifference[CC<:Currency](cc:CC): Price[CC] = cc match {
    case _:BTC.type => Price(0.0001, cc)
  }

  // even out price rate between given exchanges if possible
  def run[C <: Coin, CC <: Currency](es: SOME_EXCHANGE*)(c: C, cc: CC): Unit = {

    // gather min rate percent from exchanges (take highest rate limit as lower limit)
    val minRatePercent = es.map(_.tradeRateLimit).fold(Percent(0))((a1,a2) => Math.max(a1,a2))

    val averagePriceRate = this.averagePriceRate(c,cc)(es)

    // calc coin volume trade limits
    val coinVolumeRange = es.map(_.coinVolumeRange(c,cc))
      .fold(CoinVolumeRange(TradeVolume(Double.MinValue, averagePriceRate), TradeVolume(Double.MaxValue, averagePriceRate)))((r1,r2) =>
        CoinVolumeRange(
          TradeVolume(Math.max(r1.min.coinVolume, r2.min.coinVolume), averagePriceRate),
          TradeVolume(Math.min(r1.max.coinVolume, r2.max.coinVolume), averagePriceRate)))

    // exchange rate iterator
    val iterator: Iterator[SOME_EXCHANGERATE[C, CC]] = ExchangeRate.stream(es:_*)(c, cc, minRatePercent, minPriceDifference(cc), coinVolumeRange).iterator

    def go(): Unit = {
      val exchangeRate = iterator.next
      if (exchangeRate.orders(minRatePercent, minPriceDifference(cc), coinVolumeRange).isDefined) placeOrders(exchangeRate)
      else println(s"${LocalTime.now()} rate ${c} ${exchangeRate.from} -> ${exchangeRate.to} ${exchangeRate.highestPossibleRate}")
      scheduler.scheduleOnce(delay = 10 seconds)(go())
    }

    def printOrderInfo[E1<:Exchange[E1], E2<:Exchange[E2]](buyOrder: BuyOrder[E1, C, CC], sellOrder: SellOrder[E2, C, CC], rate: Percent) = {
      val volume = "%.4f".format(buyOrder.volume.coinVolume)
      val difference = "%.8f".format((sellOrder.price - buyOrder.price).amount)
      println(s"${LocalTime.now()} rate ${c} ${buyOrder.exchange} -> ${sellOrder.exchange} ${rate} exchanging ${volume} ${c} (price difference ${difference} $cc)")
    }

    def printOrderIdInfo[E<:Exchange[E]](exchange: Exchange[E], oid: Future[OrderId[E,C,CC]]) = {
      oid.onComplete {
        case Success(id) => println(s"placed order successfully: $id")
        case Failure(t) => println(s"failed to place order at $exchange: ${t.getMessage}")
      }
    }

    def placeOrders[E1<:Exchange[E1], E2<:Exchange[E2]](er: ExchangeRate[E1,E2,C,CC]) = {

      val rate = er.actualRate(minRatePercent, minPriceDifference(er.currency), coinVolumeRange).get
      val (buyOrder, sellOrder) = er.orders(minRatePercent, minPriceDifference(er.currency), coinVolumeRange).get

      // some assertions
      assert(buyOrder.price < sellOrder.price)
      assert(buyOrder.volume.coinVolume == sellOrder.volume.coinVolume)

      if (sufficientBalance(buyOrder) && sufficientBalance(sellOrder)) {

        // info
        printOrderInfo(buyOrder, sellOrder, rate)

        // place order
        printOrderIdInfo(buyOrder.exchange, er.from.place(order = buyOrder, immediateFillOrCancel = true))
        printOrderIdInfo(sellOrder.exchange, er.to.place(order = sellOrder, immediateFillOrCancel = true))
      }
    }

    ExchangeBalances.withExchanges(es:_*)
    println(s"${es.mkString(", ")} $c/$cc min rate $minRatePercent, min price difference ${minPriceDifference(cc)}, coin volume from ${"%.8f".format(coinVolumeRange.min.coinVolume)} (${"%.8f".format(coinVolumeRange.min.price.amount)} $cc) to ${"%.8f".format(coinVolumeRange.max.coinVolume)} (${"%.8f".format(coinVolumeRange.max.price.amount)} $cc)")
    go()
  }
}
