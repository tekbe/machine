package money.machine

import Akka._

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.unmarshalling.Unmarshal
import money.machine.ExchangeBalances.Balances
import money.machine.ExchangeRate.SOME_EXCHANGE
import spray.json.{RootJsonFormat, pimpString}

import scala.collection._
import scala.concurrent.duration.DurationLong
import scala.concurrent.{Await, Future}


// @forma.tter:off
object Bittrex extends adapter.Bittrex
object Cryptopia extends adapter.Cryptopia
object Poloniex extends adapter.Poloniex
object HitBtc extends adapter.HitBtc

/**
  * api for crypto exchanges / plugin structure for exchange adapters
  */
object Exchange {

  final val exchanges: List[SOME_EXCHANGE] = List[SOME_EXCHANGE](Cryptopia, Poloniex, Bittrex, HitBtc)
}

trait Exchange[E<:Exchange[E]] {

  val debug = false

  type OrderBookResponseType
  implicit val orderBookResponseFormat: RootJsonFormat[OrderBookResponseType]
  type BalancesResponseType
  implicit val balancesResponseFormat: RootJsonFormat[BalancesResponseType]
  type PlaceOrderResponseType
  implicit val placeOrderResponseFormat: RootJsonFormat[PlaceOrderResponseType]
  type CancelOrderResponseType
  implicit val cancelOrderResponseFormat: RootJsonFormat[CancelOrderResponseType]
  type OpenOrdersResponseType
  implicit val openOrdersResponseFormat: RootJsonFormat[OpenOrdersResponseType]

  def currencies: immutable.Seq[Currency] = tokens.collect{ case c:Currency => c }
  def coins: immutable.Seq[Coin] = tokens.collect{ case c:Coin => c }
  def tokens: immutable.Seq[Token]
  def supports(symbol: Token): Boolean = tokens.contains(symbol)

  def tradeRateLimit = Percent(0.8)

  def maxTradePrice[CC<:Currency](cc:CC): Price[CC] = cc match {
    case _:BTC.type => Price(0.025, cc)
  }

  def minTradePrice[CC<:Currency](cc:CC): Price[CC] = cc match {
    case _:BTC.type => Price(0.0002, cc)
  }

  def minCoinTradeVolume[C<:Coin,CC<:Currency](priceRate: PriceRate[C,CC]): TradeVolume[C, CC] =
    priceRate.coinVolume(minTradePrice(priceRate.currency))

  def maxCoinTradeVolume[C<:Coin,CC<:Currency](priceRate: PriceRate[C,CC]): TradeVolume[C, CC] =
    priceRate.coinVolume(maxTradePrice(priceRate.currency))

  def coinVolumeRange[C<:Coin,CC<:Currency](c:C,cc:CC): CoinVolumeRange[C,CC] = {
    val priceRate: PriceRate[C,CC] = Await.result(this.priceRate(c,cc), 10 seconds)
    CoinVolumeRange(minCoinTradeVolume(priceRate), maxCoinTradeVolume(priceRate))
  }


  def request(request: HttpRequest): Future[String] =
    Http()
      .singleRequest(request)
      .flatMap(r => Unmarshal(r.entity).to[String])
      .map { s =>
        if (debug) println(s)
        s
      }

  def orderBookRequest: Coin => Currency => HttpRequest
  def toOrderBook[C<:Coin, CC<:Currency](coin: C, currency: CC)(response: OrderBookResponseType): OrderBook[E,C,CC]
  def orderBook[C<:Coin, CC<:Currency](coin: C, currency: CC): Future[OrderBook[E,C,CC]] = {
    assert(supports(coin))
    assert(supports(currency))

    request(orderBookRequest(coin)(currency))
      .map(_.parseJson.convertTo[OrderBookResponseType])
      .map(toOrderBook(coin, currency))
  }

  /** highest bid price rate */
  def priceRate[C<:Coin, CC<:Currency](coin: C, currency: CC): Future[PriceRate[C,CC]] =
    orderBook(coin, currency).map(ob => ob.bids.head.volume.priceRate)

  def balancesRequest: HttpRequest

  def toBalances: BalancesResponseType => Balances

  def balances: Future[Balances] =
    request(balancesRequest)
      .map(s => s.parseJson.convertTo[BalancesResponseType])
      .map(toBalances)

  def toOrderId[C<:Coin, CC<:Currency]: (C,CC) => PlaceOrderResponseType => OrderId[E,C,CC]

  private def cancelIfNotFilled[C<:Coin, CC<:Currency](orderId: OrderId[E,C,CC]): Unit = {
    for {
      openOrders <- openOrders(orderId.coin, orderId.currency)
      if orderId.id != "-" && openOrders.contains(orderId)
      _ <- cancel(orderId)
    } yield ()
  }

  def placeOrderRequest[C<:Coin, CC<:Currency](order: NewOrder[E,C,CC]): HttpRequest

  def place[C<:Coin, CC<:Currency](order: NewOrder[E,C,CC], immediateFillOrCancel: Boolean = false): Future[OrderId[E,C,CC]] = {
    assert(supports(order.coin))
    assert(supports(order.currency))

    val orderId = request(placeOrderRequest(order))
      .map(s => s.parseJson.convertTo[PlaceOrderResponseType])
      .map(toOrderId(order.coin, order.currency))

    orderId.map { orderId =>
      if (immediateFillOrCancel) {
        // cancel eventually
        Akka.actorSystem.scheduler.scheduleOnce(delay = 10 seconds)(cancelIfNotFilled(orderId))
      }
      orderId
    }
  }

  def cancelOrderRequest[C<:Coin, CC<:Currency](orderId: OrderId[E,C,CC]): HttpRequest

  def cancelOrderSuccess: CancelOrderResponseType => Boolean

  def cancel[C<:Coin, CC<:Currency](orderId: OrderId[E,C,CC]): Future[Boolean] = {
    if (orderId.id == "-") Future { true } // was filled immediately
    else request(cancelOrderRequest(orderId))
      .map(s => s.parseJson.convertTo[CancelOrderResponseType])
      .map(cancelOrderSuccess)
  }

  def openOrdersRequest[C<:Coin, CC<:Currency](coin: C, currency: CC): HttpRequest

  def toOrderIds[C<:Coin, CC<:Currency]: (C,CC) => OpenOrdersResponseType => immutable.Seq[OrderId[E,C,CC]]

  def openOrders[C<:Coin, CC<:Currency](coin: C, currency: CC): Future[immutable.Seq[OrderId[E,C,CC]]] = {
    assert(supports(coin))
    assert(supports(currency))

    request(openOrdersRequest(coin, currency))
      .map(s => s.parseJson.convertTo[OpenOrdersResponseType])
      .map(toOrderIds(coin, currency))
  }

  override def toString: String = this.getClass.getSimpleName.split("\\$").last
}
