package money.machine.adapter

import java.time.LocalTime
import java.util.Locale

import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpMethods, HttpRequest}
import money.machine.ExchangeBalances.Balances
import money.machine._
import spray.json.DefaultJsonProtocol._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.collection.immutable


class Bittrex extends Exchange[Bittrex] {

  import Response._

  override def tokens: immutable.Seq[Token] = List(BTC, LTC, XZC, XMR)

  override type OrderBookResponseType = OrderBookResponse
  override val orderBookResponseFormat: RootJsonFormat[OrderBookResponseType] = jsonFormat3(OrderBookResponse)
  override type BalancesResponseType = BalancesResponse
  override val balancesResponseFormat: RootJsonFormat[BalancesResponseType] = jsonFormat3(BalancesResponse)
  override type PlaceOrderResponseType = PlaceOrderResponse
  override val placeOrderResponseFormat: RootJsonFormat[PlaceOrderResponseType] = jsonFormat3(PlaceOrderResponse)
  override type CancelOrderResponseType = CancelOrderResponse
  override val cancelOrderResponseFormat: RootJsonFormat[CancelOrderResponseType] = jsonFormat2(CancelOrderResponse)
  override type OpenOrdersResponseType = OpenOrdersResponse
  override val openOrdersResponseFormat: RootJsonFormat[OpenOrdersResponseType] = jsonFormat3(OpenOrdersResponse)

  override def minTradePrice[CC<:Currency](cc:CC): Price[CC] = cc match {
    case _:BTC.type => Price(0.00051, cc) // // "DUST_TRADE_DISALLOWED_MIN_VALUE_50K_SAT"
  }

  def privateApi(command: String, additionalParams: String = ""): HttpRequest = {
    val apiKey = ApiKeys(this)._1
    val apiSecret = ApiKeys(this)._2

    val nonce = (System.currentTimeMillis() + 3000).toString
    val url = s"https://bittrex.com/api/v1.1/$command?apikey=$apiKey&nonce=$nonce$additionalParams"
    val sign = Hex.of(Mac.sign(url, apiSecret.getBytes)).toUpperCase

    HttpRequest(
      HttpMethods.GET,
      url,
      headers = RawHeader("apisign", sign) +: Nil)
  }

  override def toOrderBook[C <: Coin, CC <: Currency](coin: C, currency: CC)(response: OrderBookResponse): OrderBook[Bittrex, C, CC] = {
    val now = LocalTime.now()
    OrderBook(
      // lowest sell price first
      asks = AskBook(Bittrex, coin, currency,
        offers = response.result.sell.take(100).map(s => Ask(this, TradeVolume(s.Quantity, PriceRate(coin, s.Rate, currency)))).sortBy(_.volume.priceRate.rate),
        time = now),
      // highest bid first
      bids = BidBook(Bittrex, coin, currency,
        offers = response.result.buy.take(100).map(b => Bid(this, TradeVolume(b.Quantity, PriceRate(coin, b.Rate, currency)))).sortBy(_.volume.priceRate.rate).reverse,
        time = now))
  }

  def orderBookRequest: Coin => Currency => HttpRequest = coin => currency =>
    HttpRequest(
      HttpMethods.GET,
      s"https://bittrex.com/api/v1.1/public/getorderbook?market=${currency}-${coin}&type=both")

  override def balancesRequest: HttpRequest = privateApi("account/getbalances")

  override def toBalances: (BalancesResponse) => Balances = r =>
    if (r.success) r.result.map(b => (Token.forName(b.Currency), b.Available))
    .filter(_._1.isDefined)
    .map { case (token, balance) => (token.get, BigDecimal(balance)) }
    .filter{ case (token, _) => supports(token) }
    .toMap
  else throw new RuntimeException(s"failed to load bittrex balances ${r.message}")

  override def toOrderId[C <: Coin, CC <: Currency]: (C, CC) => (PlaceOrderResponse) => OrderId[Bittrex, C, CC] =
    (coin, currency) => r =>
      if (r.success) OrderId(r.result.uuid.getOrElse("-"), Bittrex, coin, currency)
      else throw new RuntimeException(s"failed to place bittrex order, ${r.message}")

  override def placeOrderRequest[C <: Coin, CC <: Currency](order: NewOrder[Bittrex, C, CC]): HttpRequest = {

    val command = order match {
      case BuyOrder(_,_) => "market/buylimit"
      case SellOrder(_,_) => "market/selllimit"
    }
    val params = s"&market=${order.currency}-${order.coin}&quantity=${"%.10f".formatLocal(Locale.US, order.volume.coinVolume.doubleValue)}&rate=${"%.10f".formatLocal(Locale.US, order.priceRate.rate.doubleValue)}"
    privateApi(command, params)
  }

  override def cancelOrderRequest[C <: Coin, CC <: Currency](orderId: OrderId[Bittrex, C, CC]): HttpRequest =
    privateApi("market/cancel", s"&uuid=${orderId.id}")

  override def cancelOrderSuccess: (Response.CancelOrderResponse) => Boolean = r => r.success

  override def openOrdersRequest[C <: Coin, CC <: Currency](coin: C, currency: CC): HttpRequest =
    privateApi("market/getopenorders",s"&market=$currency-$coin")

  override def toOrderIds[C <: Coin, CC <: Currency]: (C, CC) => (OpenOrdersResponseType) => immutable.Seq[OrderId[Bittrex, C, CC]] =
    (coin, currency) => r => r.result.map(id => OrderId(id.OrderUuid.getOrElse("-"), Bittrex, coin, currency))

  object Response {

    case class OrderBookResponse(success: Boolean, message: Option[String], result: OrderBookData)
    case class OrderBookData(buy: Vector[OrderBookOrder], sell: Vector[OrderBookOrder])
    case class OrderBookOrder(Rate: Double, Quantity: Double)

    implicit val orderBookOrderFormat: RootJsonFormat[OrderBookOrder] = jsonFormat2(OrderBookOrder)
    implicit val orderBookDataFormat: RootJsonFormat[OrderBookData] = jsonFormat2(OrderBookData)

    case class BalancesResponse(success: Boolean, message: Option[String], result: Vector[Balance])
    case class Balance(Currency: String, Available: Double)

    implicit val balanceFormat: RootJsonFormat[Balance] = jsonFormat2(Balance)
    implicit val balancesDataFormat: RootJsonFormat[Vector[Balance]] = DefaultJsonProtocol.vectorFormat

    case class OpenOrdersResponse(success: Boolean, message: Option[String], result: Vector[OrderIdResponse])
    case class OrderIdResponse(OrderUuid: Option[String])

    implicit val orderIdFormat: RootJsonFormat[OrderIdResponse] = jsonFormat1(OrderIdResponse)

    case class PlaceOrderResponse(success: Boolean, message: Option[String], result: NewOrderIdResponse)
    case class NewOrderIdResponse(uuid: Option[String])

    implicit val newOrderIdFormat: RootJsonFormat[NewOrderIdResponse] = jsonFormat1(NewOrderIdResponse)

    case class CancelOrderResponse(success: Boolean, message: Option[String])


  }
}
