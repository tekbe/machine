import java.time.{Duration, LocalTime}
import java.time.temporal.ChronoUnit

import money.machine._

import Math._
import Akka._

import org.scalatest.FunSuite
import money.machine.{ARDR, BTC, Bittrex, Coin, Cryptopia, Currency, Exchange, HitBtc, LTC, Poloniex, XSPEC}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.util.{Success, Try}


class MachineTest extends FunSuite {

  test("combine futures") {

    val f: Future[Seq[Int]] = Combine.combineFutures(List(Future(1), Future(2), Future(3)))

    f.onComplete{
      case Success(v) => assert(v === List(1,2,3))
      case _ => fail()
    }

    val f2: Future[Seq[Int]] = Combine.combineFutures(Nil)

    f2.onComplete{
      case Success(v) => assert(v === Nil)
      case _ => fail()
    }
  }

  test("time") {
    val now = LocalTime.now()
    assert(Time.sameTime(now, now, 0))
    assert(Time.sameTime(now, now.plusSeconds(1), 2000))
    assert(!Time.sameTime(now, LocalTime.MAX, 0))
    assert(!Time.sameTime(now, LocalTime.MIN, 0))
  }

  test("percent") {
    assert(Percent(2.5).toString === "2,50%")
    assert(Percent(25.5).toString === "25,50%")
    assert(Percent(-1.5).toString === "-1,50%")
    assert(Percent(-1.5) < Percent(2.5))
    assert(Percent(2.5) <= Percent(2.5))

    assert((Percent(22) of 200) === Percent(44))
  }

  test("trade ranges for all coins/BTC") {
    Exchange.exchanges.foreach { e =>
      assert(e.minTradePrice(BTC) <= e.maxTradePrice(BTC))

      e.coins.foreach{c =>
        val rate = Await.result(e.priceRate(c,BTC), 10 seconds)
        assert(e.minCoinTradeVolume(rate).coinVolume <= e.maxCoinTradeVolume(rate).coinVolume)
      }
    }
  }

  test("price range / coin volume range") {
    assert(Try{CoinVolumeRange(TradeVolume(20, PriceRate(LTC,0.1,BTC)), TradeVolume(10, PriceRate(LTC,0.1,BTC)))}.toOption.isEmpty)
    assert(Try{CoinVolumeRange(TradeVolume(10, PriceRate(LTC,0.2,BTC)), TradeVolume(10, PriceRate(LTC,0.1,BTC)))}.toOption.isEmpty)
    assert(Try{CoinVolumeRange(TradeVolume(10, PriceRate(LTC,0.1,BTC)), TradeVolume(10, PriceRate(LTC,0.1,BTC)))}.toOption.isDefined)
    assert(Try{PriceRange(Price(20, BTC), Price(10, BTC))}.toOption.isEmpty)
    assert(Try{PriceRange(Price(10, BTC), Price(10, BTC))}.toOption.isDefined)

    val range = PriceRange(Price(0.1, BTC), Price(10, BTC))
    val rate = PriceRate(LTC, 0.001, BTC)
    assert(range.toCoinVolumeRange(rate) == CoinVolumeRange(TradeVolume(100, PriceRate(LTC, 0.001, BTC)), TradeVolume(10000, PriceRate(LTC, 0.001, BTC))))
  }

  test("trade volume") {
    val volume = TradeVolume(5000, PriceRate(LTC, 0.1, BTC))
    assert(volume.price === Price(500, BTC))
    assert(volume.take(1000).priceRate === PriceRate(LTC, 0.1, BTC))
    assert(volume.take(1000).coinVolume === 1000)
    assert(volume.take(1000).price === Price(100, BTC))
  }

  test("price") {
    assert(Price(0, BTC) + Price(10, BTC) === Price(10, BTC))
    assert(Price(10, BTC) - Price(1, BTC) === Price(9, BTC))
    assert(Price(10, BTC) > Price(1, BTC))
    assert(Price(1, BTC) == Price(1, BTC))
    assert(Price(0.1, BTC) <= Price(1, BTC))
  }

  test("price rate ordering") {

    assert(Try(PriceRate(LTC, 0, BTC)).toOption.isEmpty)
    assert(Try(PriceRate(LTC, -0.1, BTC)).toOption.isEmpty)

    assert(PriceRate(LTC, 0.4, BTC) > PriceRate(LTC, 0.3, BTC))
    assert(PriceRate(LTC, 0.2, BTC) <= PriceRate(LTC, 0.3, BTC))
    assert(PriceRate(LTC, 0.1, BTC) == PriceRate(LTC, 0.1, BTC))
  }

  test("coin volume for price/price rate volume") {

    val rate1 = PriceRate(LTC, 10, BTC)

    assert(rate1.coinVolume(Price(5,BTC)).priceRate === rate1)
    assert(rate1.coinVolume(Price(5,BTC)).coinVolume === 0.5)

    val rate2 = PriceRate(LTC, 0.1, BTC)

    assert(rate2.coinVolume(Price(5,BTC)).priceRate === rate2)
    assert(rate2.coinVolume(Price(5,BTC)).coinVolume === 50)
  }

  test("ask book / bid book") {
    // empty
    assert(AskBook(Cryptopia, LTC, BTC, Nil).isEmpty)
    assert(BidBook(Cryptopia, LTC, BTC, Nil).isEmpty)
    // wrong sorting
    assert(Try(AskBook(Cryptopia, LTC, BTC,
      Ask(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.2, BTC))) ::
      Ask(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.1, BTC))) :: Nil)).toOption.isEmpty)
    assert(Try(BidBook(Cryptopia, LTC, BTC,
      Bid(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.1, BTC))) ::
      Bid(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.2, BTC))) :: Nil)).toOption.isEmpty)

    val ab = AskBook(Cryptopia, LTC, BTC,
      Ask(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.1, BTC))) ::
      Ask(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.2, BTC))) :: Nil)

    // not empty
    assert(!ab.isEmpty)
    // availability
    assert(ab.coinsAvailable === 200)
    assert(ab.priceAvailable === Price(10, BTC) + Price(20, BTC))
    // coin volume
    assert(ab.coinsAvailable() === 200)
    assert(ab.coinVolume(PriceRate(LTC, 0.01, BTC)) === 0)
    assert(ab.coinVolume(PriceRate(LTC, 0.11, BTC)) === 100)
    assert(ab.coinVolume(PriceRate(LTC, 0.2, BTC)) === 200)
    // tail
    assert(ab.tail.coinsAvailable() === 100)
    assert(ab.tail.coinVolume(PriceRate(LTC, 0.11, BTC)) === 0)
    assert(ab.tail.coinVolume(PriceRate(LTC, 0.21, BTC)) === 100)
    assert(ab.tail.tail.isEmpty)
    // price rate
    assert(ab.priceRate(100) === PriceRate(LTC, 0.1, BTC))
    assert(ab.priceRate(101) === PriceRate(LTC, 0.2, BTC))
    assert(Try(ab.priceRate(201)).toOption.isEmpty)
    // price
    assert(Try(ab.price(201)).toOption.isEmpty)
    assert(ab.price(0) === Price(0, BTC))
    assert(ab.price(150) === Price(100 * 0.1, BTC) + Price(50 * 0.2, BTC))
    // time
    assert(ab.sameTime(ab))
    assert(!ab.sameTime(AskBook(Cryptopia, LTC, BTC, Nil, time = LocalTime.MAX)))
    // order by volume
    assert(Try(ab.createOrder(0)).toOption.isEmpty)
    assert(Try(ab.createOrder(201)).toOption.isEmpty)
    assert(ab.createOrder(99) === BuyOrder(Cryptopia, TradeVolume(99, PriceRate(LTC, 0.1, BTC))))
    assert(ab.createOrder(101) === BuyOrder(Cryptopia, TradeVolume(101, PriceRate(LTC, 0.2, BTC))))
    // order by price
    assert(Try(ab.createOrder(Price(0, BTC))).toOption.isEmpty)
    assert(Try(ab.createOrder(Price(30.1, BTC))).toOption.isEmpty)
    assert(ab.createOrder(Price(9.9, BTC)) === BuyOrder(Cryptopia, TradeVolume(99, PriceRate(LTC, 0.1, BTC))))
    assert(ab.createOrder(Price(10.1, BTC)) === BuyOrder(Cryptopia, TradeVolume(100.5, PriceRate(LTC, 0.2, BTC))))


    val bb = BidBook(Cryptopia, LTC, BTC,
      Bid(Cryptopia, TradeVolume(110, PriceRate(LTC, 0.2, BTC))) ::
      Bid(Cryptopia, TradeVolume(200, PriceRate(LTC, 0.1, BTC))) :: Nil)

    // not empty
    assert(!bb.isEmpty)
    // availability
    assert(bb.coinsAvailable === 310)
    assert(bb.priceAvailable === Price(42, BTC))
    // coin volume
    assert(bb.coinsAvailable() === 310)
    assert(bb.coinVolume(PriceRate(LTC, 0.01, BTC)) === 310)
    assert(bb.coinVolume(PriceRate(LTC, 0.11, BTC)) === 110)
    assert(bb.coinVolume(PriceRate(LTC, 0.2, BTC)) === 110)
    assert(bb.coinVolume(PriceRate(LTC, 0.21, BTC)) === 0)
    // tail
    assert(bb.tail.coinsAvailable() === 200)
    assert(bb.tail.coinVolume(PriceRate(LTC, 0.09, BTC)) === 200)
    assert(bb.tail.coinVolume(PriceRate(LTC, 0.21, BTC)) === 0)
    assert(bb.tail.tail.isEmpty)
    // price rate
    assert(bb.priceRate(100) === PriceRate(LTC, 0.2, BTC))
    assert(bb.priceRate(111) === PriceRate(LTC, 0.1, BTC))
    assert(Try(bb.priceRate(311)).toOption.isEmpty)
    // price
    assert(Try(ab.price(311)).toOption.isEmpty)
    assert(bb.price(0) === Price(0, BTC))
    assert(bb.price(200) === Price(110 * 0.2, BTC) + Price(90 * 0.1, BTC))
    // time
    assert(bb.sameTime(bb))
    assert(!bb.sameTime(BidBook(Cryptopia, LTC, BTC, Nil, time = LocalTime.MAX)))
    // order by volume
    assert(Try(bb.createOrder(0)).toOption.isEmpty)
    assert(Try(bb.createOrder(311)).toOption.isEmpty)
    assert(bb.createOrder(100) === SellOrder(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.2, BTC))))
    assert(bb.createOrder(111) === SellOrder(Cryptopia, TradeVolume(111, PriceRate(LTC, 0.1, BTC))))
    // order by price
    assert(Try(bb.createOrder(Price(0, BTC))).toOption.isEmpty)
    assert(Try(bb.createOrder(Price(42.1, BTC))).toOption.isEmpty)
    assert(bb.createOrder(Price(20, BTC)) === SellOrder(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.2, BTC))))
    assert(bb.createOrder(Price(22.1, BTC)) === SellOrder(Cryptopia, TradeVolume(111, PriceRate(LTC, 0.1, BTC))))
  }

  test("exchange rate for same time only") {

    val difference: Duration = LTC.timeDifferenceTolerance.plusMillis(1)

    val ob1 = OrderBook(AskBook(Cryptopia, LTC, BTC, Ask(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.1, BTC))) :: Nil, time = LocalTime.MIN), BidBook(Cryptopia, LTC, BTC, Bid(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.1, BTC))) :: Nil, time = LocalTime.MIN))
    val ob2 = OrderBook(AskBook(Cryptopia, LTC, BTC, Ask(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.1, BTC))) :: Nil, time = LocalTime.MIN.plus(difference)), BidBook(Cryptopia, LTC, BTC, Bid(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.1, BTC))) :: Nil, time = LocalTime.MIN.plus(difference)))
    assert(
      Try(ExchangeRate(ob1.asks, ob2.bids)).toOption === None
    )
    assert(
      Try(ExchangeRate(ob2.asks, ob1.bids)).toOption === None
    )
  }

  test("percentage diff") {
    assert(ratePercent(0, 0) === Percent(0))
    assert(ratePercent(math.Pi, math.Pi) === Percent(0.0))
    assert(ratePercent(math.Pi, math.Pi * 2) === Percent(-50))
    assert(ratePercent(math.Pi, math.Pi * 4) === Percent(-75))
    assert(ratePercent(90, 100) === Percent(-10))
    assert(ratePercent(110, 100) === Percent(10))
    assert(ratePercent(0, 100) === Percent(-100))
    assert(ratePercent(PriceRate(LTC, 0.5, BTC), PriceRate(LTC, 0.4, BTC)) === Percent(25))
    assert(ratePercent(Price(5, BTC), Price(4, BTC)) === Percent(25))
  }

  test("order book") {

    // empty books
    assert(Try(OrderBook(asks = AskBook(Cryptopia, LTC, BTC, offers = Nil), bids = BidBook(Cryptopia, LTC, BTC, offers = Nil))).toOption === None)
    // different time
    assert(Try(OrderBook(
      asks = AskBook(Cryptopia, LTC, BTC, offers = Ask(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.1, BTC))) :: Nil, time = LocalTime.MIN),
      bids = BidBook(Cryptopia, LTC, BTC, offers = Bid(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.01, BTC))) :: Nil, time = LocalTime.MAX)
    )).toOption === None)
  }

  test("exchange rate") {
    val orderBook1 = OrderBook(
      asks = AskBook(Cryptopia, LTC, BTC, offers = Ask(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.1, BTC))) :: Ask(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.2, BTC))) :: Nil),
      bids = BidBook(Cryptopia, LTC, BTC, offers = Bid(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.05, BTC))) :: Bid(Cryptopia, TradeVolume(100, PriceRate(LTC, 0.01, BTC))) :: Nil)
    )

    // same exchange
    val er1 = ExchangeRate(orderBook1.asks, orderBook1.bids)
    // no orders
    assert(er1.orders(Percent(0), Price(0, BTC), CoinVolumeRange(TradeVolume(0, PriceRate(LTC, 0.1,BTC)), TradeVolume(Long.MaxValue, PriceRate(LTC, 0.1,BTC)))).isEmpty)
    // no rate
    assert(er1.actualRate(Percent(0), Price(0, BTC), CoinVolumeRange(TradeVolume(0, PriceRate(LTC, 0.1,BTC)), TradeVolume(Long.MaxValue, PriceRate(LTC, 0.1,BTC)))).isEmpty)
    // compare 0.05 to 0.1
    assert(er1.highestPossibleRate === Percent(-50))

    val orderBook2 = OrderBook(
      asks = AskBook(Poloniex, LTC, BTC, offers = Ask(Poloniex, TradeVolume(50, PriceRate(LTC, 0.121, BTC))) :: Ask(Poloniex, TradeVolume(25, PriceRate(LTC, 0.2, BTC))) :: Nil),
      bids = BidBook(Poloniex, LTC, BTC, offers = Bid(Poloniex, TradeVolume(50, PriceRate(LTC, 0.12, BTC))) :: Bid(Poloniex, TradeVolume(25, PriceRate(LTC, 0.11, BTC))) :: Nil)
    )

    val er = ExchangeRate(orderBook1.asks, orderBook2.bids)

    val volumeUnlimited = CoinVolumeRange(TradeVolume(0, PriceRate(LTC, 0.1,BTC)), TradeVolume(Long.MaxValue, PriceRate(LTC, 0.1,BTC)))

    assert(er.highestPossibleRate === Percent(20))

    assert(er.orders(Percent(0), Price(0, BTC), volumeUnlimited).isDefined)
    assert(er.actualRate(Percent(0), Price(0, BTC), volumeUnlimited).isDefined)

    assert(er.orders(Percent(0), Price(0, BTC), volumeUnlimited).get._1.volume.coinVolume === 75)
    assert(er.orders(Percent(0), Price(0, BTC), volumeUnlimited).get._1.volume.priceRate === PriceRate(LTC, 0.1, BTC))
    assert(er.orders(Percent(0), Price(0, BTC), volumeUnlimited).get._1.exchange === Cryptopia)
    assert(er.orders(Percent(0), Price(0, BTC), volumeUnlimited).get._2.volume.coinVolume === 75)
    assert(er.orders(Percent(0), Price(0, BTC), volumeUnlimited).get._2.volume.priceRate === PriceRate(LTC, 0.11, BTC))
    assert(er.orders(Percent(0), Price(0, BTC), volumeUnlimited).get._2.exchange === Poloniex)
    assert(er.actualRate(Percent(0), Price(0, BTC), volumeUnlimited).get === Math.ratePercent(Price(50 * 0.12, BTC) + Price(25 * 0.11, BTC), Price(75 * 0.1, BTC)))

    // set rate limit
    assert(er.orders(Percent(20), Price(0, BTC), volumeUnlimited).get._1.volume.coinVolume === 50)
    assert(er.orders(Percent(20), Price(0, BTC), volumeUnlimited).get._1.volume.priceRate === PriceRate(LTC, 0.1, BTC))
    assert(er.orders(Percent(20), Price(0, BTC), volumeUnlimited).get._1.exchange === Cryptopia)
    assert(er.orders(Percent(20), Price(0, BTC), volumeUnlimited).get._2.volume.coinVolume === 50)
    assert(er.orders(Percent(20), Price(0, BTC), volumeUnlimited).get._2.volume.priceRate === PriceRate(LTC, 0.12, BTC))
    assert(er.orders(Percent(20), Price(0, BTC), volumeUnlimited).get._2.exchange === Poloniex)
    assert(er.actualRate(Percent(20), Price(0, BTC), volumeUnlimited).get === Math.ratePercent(Price(50 * 0.12, BTC).amount, Price(50 * 0.1, BTC).amount))
    assert(er.actualPriceDifference(Percent(20), Price(0, BTC), volumeUnlimited).get === Price(50 * 0.12, BTC) - Price(50 * 0.1, BTC))


    // set upper volume limit
    val volumeLimited = CoinVolumeRange(TradeVolume(0, PriceRate(LTC, 0.1,BTC)), TradeVolume(60, PriceRate(LTC, 0.1,BTC)))

    assert(er.orders(Percent(0), Price(0, BTC), volumeLimited).get._1.volume.coinVolume === 60)
    assert(er.orders(Percent(0), Price(0, BTC), volumeLimited).get._1.volume.priceRate === PriceRate(LTC, 0.1, BTC))
    assert(er.orders(Percent(0), Price(0, BTC), volumeLimited).get._1.exchange === Cryptopia)
    assert(er.orders(Percent(0), Price(0, BTC), volumeLimited).get._2.volume.coinVolume === 60)
    assert(er.orders(Percent(0), Price(0, BTC), volumeLimited).get._2.volume.priceRate === PriceRate(LTC, 0.11, BTC))
    assert(er.orders(Percent(0), Price(0, BTC), volumeLimited).get._2.exchange === Poloniex)
    assert(er.actualRate(Percent(0), Price(0, BTC), volumeLimited).get === Math.ratePercent((Price(50 * 0.12, BTC) + Price(10 * 0.11, BTC)).amount, Price(60 * 0.1, BTC).amount))
    assert(er.actualPriceDifference(Percent(0), Price(0, BTC), volumeLimited).get === (Price(50 * 0.12, BTC) + Price(10 * 0.11, BTC)) - Price(60 * 0.1, BTC))

    // price difference not reached
    assert(er.orders(Percent(0), Price(10, BTC), volumeLimited).isEmpty)

    // lower volume limit and rate limit not satisfiable
    val volumeLowerLimit = CoinVolumeRange(TradeVolume(60, PriceRate(LTC, 0.1,BTC)), TradeVolume(Double.MaxValue, PriceRate(LTC, 0.1,BTC)))

    assert(er.orders(Percent(20), Price(0, BTC), volumeLowerLimit).isEmpty)
    assert(er.actualRate(Percent(20), Price(0, BTC), volumeLowerLimit).isEmpty)
  }

  test("exchange rate negative on same exchange -> None") {

    def go[E<:Exchange[E]](e:Exchange[E]) = {
      e.coins.foreach { c =>

        val er = Await.result(for {
          ob <- e.orderBook(c, BTC)
        } yield ExchangeRate(ob.asks, ob.bids), 25 seconds)

        assert(er.highestPossibleRate < Percent(0))
      }
    }

    go(Cryptopia)
    go(Poloniex)
    go(Bittrex)
  }

  test("order book: transform cryptopia responses") {

    val response = Cryptopia.Response.OrderBookResponse(true, None,
      Cryptopia.Response.OrderBookData(
        Buy = Vector(
          Cryptopia.Response.OrderBookOrder(Price = 0.01, Volume = 100.1),
          Cryptopia.Response.OrderBookOrder(Price = 0.03, Volume = 10.1),
          Cryptopia.Response.OrderBookOrder(Price = 0.02, Volume = 20.1)),
        Sell = Vector(
          Cryptopia.Response.OrderBookOrder(Price = 0.3, Volume = 91.5),
          Cryptopia.Response.OrderBookOrder(Price = 0.2, Volume = 90.5)
          )))

    val orderBook = Cryptopia.toOrderBook(LTC, BTC)(response)

    assert(orderBook.exchange === Cryptopia)
    assert(orderBook.coin === LTC)
    assert(ChronoUnit.MINUTES.between(orderBook.time, LocalTime.now()) === 0)
    assert(orderBook.bids.size === 3)
    assert(orderBook.bids.head === Bid(Cryptopia, TradeVolume(10.1, PriceRate(LTC, 0.03, BTC))))
    assert(orderBook.asks.size === 2)
    assert(orderBook.asks.head === Ask(Cryptopia, TradeVolume(90.5, PriceRate(LTC, 0.2, BTC))))
  }

  test("order book: transform poloniex response") {
    val response = Poloniex.Response.OrderBookResponse(bids = Vector(("0.01", 100.1), ("0.03", 10.1), ("0.02", 20.1)), asks = Vector(("0.3", 91.5), ("0.2", 90.5)))

    val orderBook = Poloniex.toOrderBook(ARDR, BTC)(response)

    assert(orderBook.exchange === Poloniex)
    assert(orderBook.coin === ARDR)
    assert(ChronoUnit.MINUTES.between(orderBook.time, LocalTime.now()) === 0)
    assert(orderBook.bids.size === 3)
    assert(orderBook.bids.head === Bid(Poloniex, TradeVolume(10.1, PriceRate(ARDR, 0.03, BTC))))
    assert(orderBook.asks.size === 2)
    assert(orderBook.asks.head === Ask(Poloniex, TradeVolume(90.5, PriceRate(ARDR, 0.2, BTC))))
  }

  test("order book: transform hitbtc response") {
    val response = HitBtc.Response.OrderBookResponse(asks = Vector((0.3, 91.5), (0.2, 90.5)), bids = Vector((0.01, 100.1), (0.03, 10.1), (0.02, 20.1)))
    val orderBook = HitBtc.toOrderBook(ARDR, BTC)(response)

    assert(orderBook.exchange === HitBtc)
    assert(orderBook.coin === ARDR)
    assert(ChronoUnit.MINUTES.between(orderBook.time, LocalTime.now()) === 0)
    assert(orderBook.bids.size === 3)
    assert(orderBook.bids.head === Bid(HitBtc, TradeVolume(10.1, PriceRate(ARDR, 0.03, BTC))))
    assert(orderBook.asks.size === 2)
    assert(orderBook.asks.head === Ask(HitBtc, TradeVolume(90.5, PriceRate(ARDR, 0.2, BTC))))
  }

  test("order book: transform bittrex response") {
    val response = Bittrex.Response.OrderBookResponse(true, None,
      Bittrex.Response.OrderBookData(
        buy = Vector(
          Bittrex.Response.OrderBookOrder(Rate = 0.01, Quantity = 100.1),
          Bittrex.Response.OrderBookOrder(Rate = 0.03, Quantity = 10.1),
          Bittrex.Response.OrderBookOrder(Rate = 0.02, Quantity = 20.1)),
        sell = Vector(
          Bittrex.Response.OrderBookOrder(Rate = 0.3, Quantity = 91.5),
          Bittrex.Response.OrderBookOrder(Rate = 0.2, Quantity = 90.5)
        )))

    val orderBook = Bittrex.toOrderBook(LTC, BTC)(response)

    assert(orderBook.exchange === Bittrex)
    assert(orderBook.coin === LTC)
    assert(ChronoUnit.MINUTES.between(orderBook.time, LocalTime.now()) === 0)
    assert(orderBook.bids.size === 3)
    assert(orderBook.bids.head === Bid(Bittrex, TradeVolume(10.1, PriceRate(LTC, 0.03, BTC))))
    assert(orderBook.asks.size === 2)
    assert(orderBook.asks.head === Ask(Bittrex, TradeVolume(90.5, PriceRate(LTC, 0.2, BTC))))

  }

  test("order book/price: orders size and sorting") {

    def go[E<:Exchange[E]](e:Exchange[E]): Unit = {

      e.coins.foreach { c =>

        val (orderBook, price) = Await.result(
          for {
            ob <- e.orderBook(c, BTC)
            p <- e.priceRate(c, BTC)
          } yield (ob, p), 60 seconds)

        assert(orderBook.bids.size >= 25 && orderBook.bids.size <= 100)
        assert(orderBook.asks.size >= 25 && orderBook.asks.size <= 100)
        assert(Sorting.isSortedDescending(orderBook.bids.offers.map(_.volume.priceRate)))
        assert(Sorting.isSortedAscending(orderBook.asks.offers.map(_.volume.priceRate)))
        assert(ratePercent(orderBook.bids.head.volume.priceRate.rate, price.rate) < Percent(1.0))
      }
    }

    go(Cryptopia)
    go(Poloniex)
    go(Bittrex)
  }

  test("balances: transform cryptopia response") {

    val response = Cryptopia.Response.BalancesResponse(true, None,
      Vector(
        Cryptopia.Response.Balance("BTC", 0.09),
        Cryptopia.Response.Balance("LTC", 25.1),
        Cryptopia.Response.Balance("ARDR", 2.1),
        Cryptopia.Response.Balance("XSPEC", 1.1)))

    val balances = Cryptopia.toBalances(response)

    assert(balances.size === 3) // ARDR not supported by cryptopia
    assert(balances.get(BTC).get === 0.09)
    assert(balances.get(LTC).get === 25.1)
    assert(balances.get(XSPEC).get === 1.1)
  }

  test("balances: transform poloniex response") {
    val response: Poloniex.BalancesResponseType = Map("BTC" -> "0.09", "LTC" -> "25.1", "ARDR" -> "2.1", "KOBO" -> "0.01")
    val balances = Poloniex.toBalances(response)

    assert(balances.size === 3) // KOBO not supported by polo
    assert(balances.get(BTC).get === 0.09)
    assert(balances.get(LTC).get === 25.1)
    assert(balances.get(ARDR).get === 2.1)
  }

  test("balances: transform hitBtc response") {
    val response: HitBtc.BalancesResponseType =
      HitBtc.Response.BalancesResponse(Vector(
        HitBtc.Response.BalanceResponse("BTC", 0.10, 0.01),
        HitBtc.Response.BalanceResponse("LTC", 25.1, 0),
        HitBtc.Response.BalanceResponse("ARDR", 2.1, 0),
        HitBtc.Response.BalanceResponse("KOBO", 0.01, 0)
      ))
    val balances = HitBtc.toBalances(response)

    assert(balances.size === 2) // KOBO and ARDR not supported by hitbtc
    assert(balances.get(BTC).get === 0.09)
    assert(balances.get(LTC).get === 25.1)
  }


  test("balances: transform bittrex response") {
    val response: Bittrex.BalancesResponseType = Bittrex.Response.BalancesResponse(true, None, Vector(
      Bittrex.Response.Balance("BTC", 0.09),
      Bittrex.Response.Balance("LTC", 25.1),
      Bittrex.Response.Balance("ARDR", 2.1),
      Bittrex.Response.Balance("XSPEC", 1.1)
    ))
    val balances = Bittrex.toBalances(response)
    assert(balances.size === 2) // ARDR, XSPEC not supported by bittrex
    assert(balances.get(BTC).get === 0.09)
    assert(balances.get(LTC).get === 25.1)
  }

  test("balances") {
    Exchange.exchanges.foreach{ e =>
      val balances = Await.result(e.balances, 60 seconds)
      assert(balances.size === e.tokens.size)
      e.tokens.foreach{ s =>
        assert(balances.get(s).isDefined)
      }
    }
  }

  test("order: place/get-open/cancel order round-trip ") {

    def orderRoundTrip[E<:Exchange[E], C<:Coin, CC<:Currency](exchange: Exchange[E], coin: C, currency: CC): Unit = {

      val minCoinVolume: BigDecimal = exchange.coinVolumeRange(coin,currency).min.coinVolume

      val balances = Await.result(exchange.balances, 25 seconds)
      val currentPriceRate = Await.result(exchange.priceRate(coin,currency), 25 seconds)

      Thread.sleep(1000)

      // create buy order
      val buyPriceRate = PriceRate(coin, currentPriceRate.rate / 10.0, currency) // very low buy price
      val buyOrder = BuyOrder(exchange, TradeVolume(minCoinVolume, buyPriceRate))

      // create sell order
      val sellPriceBTC = PriceRate(coin, currentPriceRate.rate * 10.0, currency) // very high sell price
      val sellOrder = SellOrder(exchange, TradeVolume(minCoinVolume, sellPriceBTC))

      // check balances
      assert(balances.get(currency).get > buyOrder.volume.price.amount)
      assert(balances.get(coin).get > sellOrder.volume.coinVolume)

      // place orders
      val buyOrderId: OrderId[E, C, CC] = Await.result(exchange.place(buyOrder), 35 seconds)
      val sellOrderId: OrderId[E, C, CC] = Await.result(exchange.place(sellOrder), 35 seconds)

      assert(buyOrderId.id != "-")
      assert(sellOrderId.id != "-")
      assert(buyOrderId != sellOrderId)

      Thread.sleep(9000)

      // check orders were placed
      val openOrderIds = Await.result(exchange.openOrders(coin, BTC), 35 seconds)

      assert(openOrderIds.contains(buyOrderId))
      assert(openOrderIds.contains(sellOrderId))

      Thread.sleep(1000)

      // cancel orders again
      val cancelBuy = Await.result(exchange.cancel(buyOrderId), 35 seconds)
      val cancelSell = Await.result(exchange.cancel(sellOrderId), 35 seconds)

      assert(cancelBuy)
      assert(cancelSell)

      Thread.sleep(5000)

      // check orders were canceled
      val openOrderIdsUpdate = Await.result(exchange.openOrders(coin, currency), 35 seconds)

      assert(!openOrderIdsUpdate.contains(buyOrderId))
      assert(!openOrderIdsUpdate.contains(sellOrderId))

      Thread.sleep(5000)

      // again: immediate or cancel
      val r1 = Await.result(exchange.place(buyOrder, immediateFillOrCancel = true), 35 seconds)
      val r2 = Await.result(exchange.place(sellOrder, immediateFillOrCancel = true), 35 seconds)

      // sleep and check if canceled
      Thread.sleep(25000)

      val openOrders = Await.result(exchange.openOrders(coin, currency), 25 seconds)

      assert(!openOrders.contains(r1))
      assert(!openOrders.contains(r2))
    }

//    println(s"round trip $Cryptopia")
//    orderRoundTrip(Cryptopia, LTC, testTradeSize = Price(0.0002, BTC))
//    println(s"round trip $Poloniex")
//    orderRoundTrip(Poloniex, LTC, testTradeSize = Price(0.0002, BTC))
//    println(s"round trip $Bittrex")
//    orderRoundTrip(Bittrex, LTC, testTradeSize = Price(0.00051, BTC)) // "DUST_TRADE_DISALLOWED_MIN_VALUE_50K_SAT"
//    println(s"round trip $HitBtc")
//    orderRoundTrip(HitBtc, LTC, BTC)
  }

}
