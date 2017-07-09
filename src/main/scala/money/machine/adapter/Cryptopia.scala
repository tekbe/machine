package money.machine.adapter

import java.net.URLEncoder
import java.time.LocalTime
import java.util.Locale

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import spray.json.{DefaultJsonProtocol, RootJsonFormat}
import DefaultJsonProtocol._
import money.machine.ExchangeBalances.Balances
import money.machine._

import scala.collection.immutable

// @formatter:off
class Cryptopia extends Exchange[Cryptopia] {

  import Response._

  override val tokens: immutable.Seq[Token] = List(BTC, DASH, XMR, ZEC, LTC, XSPEC, HUSH, KOBO)

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

  override def toOrderBook[C<:Coin, CC<:Currency](coin: C, currency: CC)(response: OrderBookResponse): OrderBook[Cryptopia,C,CC] = {
    val now = LocalTime.now()
    OrderBook(
      // lowest sell price first
      asks = AskBook(Cryptopia, coin, currency,
        offers = response.Data.Sell.map(s => Ask(this, TradeVolume(s.Volume, PriceRate(coin, s.Price, currency)))).sortBy(_.volume.priceRate.rate),
        time = now),
      // highest bid first
      bids = BidBook(Cryptopia, coin, currency,
        offers = response.Data.Buy.map(b => Bid(this, TradeVolume(b.Volume, PriceRate(coin, b.Price, currency)))).sortBy(_.volume.priceRate.rate).reverse,
        time = now))
  }

  def orderBookRequest: Coin => Currency => HttpRequest = coin => currency =>
    HttpRequest(
      HttpMethods.GET,
      s"https://www.cryptopia.co.nz/api/GetMarketOrders/${coin}_${currency}/100")

  override def balancesRequest: HttpRequest = privateApi("GetBalance", "{}")

  override def toBalances: BalancesResponseType => Balances = r =>
    if (r.Success) r.Data.map(b => (Token.forName(b.Symbol), b.Available))
      .filter(_._1.isDefined)
      .map { case (token, balance) => (token.get, BigDecimal(balance)) }
      .filter{ case (token, _) => supports(token) }
      .toMap
    else throw new RuntimeException(s"failed to load cryptopia balances, ${r.Error}")

  override def toOrderId[C<:Coin, CC<:Currency]: (C,CC) => (PlaceOrderResponse) => OrderId[Cryptopia, C, CC] =
    (coin, currency) => r =>
      if (r.Success) OrderId(r.Data.OrderId.map(_.toString).getOrElse("-"), Cryptopia, coin, currency)
      else throw new RuntimeException("failed to place cryptopia order")

  override def placeOrderRequest[C <: Coin, CC <: Currency](order: NewOrder[Cryptopia, C, CC]): HttpRequest = {

    val orderType: String = order match {
      case BuyOrder(_,_) => "Buy"
      case SellOrder(_,_) => "Sell"
    }
    val jsonParams = s"""{\"Market\": \"${order.coin}/${order.currency}\", \"Type\": \"${orderType}\", \"Rate\": ${"%.10f".formatLocal(Locale.US, order.volume.priceRate.rate.doubleValue)}, \"Amount\": ${"%.10f".formatLocal(Locale.US, order.volume.coinVolume.doubleValue)}"""
    privateApi("SubmitTrade", jsonParams)
  }

  override def cancelOrderRequest[C <: Coin, CC <: Currency](orderId: OrderId[Cryptopia, C, CC]): HttpRequest =
    privateApi("CancelTrade", s"""{\"Type\": \"Trade\", \"OrderId\": ${orderId.id}}""")

  override def cancelOrderSuccess: (Response.CancelOrderResponse) => Boolean = r => r.Success

  override def openOrdersRequest[C <: Coin, CC <: Currency](coin: C, currency: CC): HttpRequest =
    privateApi("GetOpenOrders", s"""{\"Market\": \"${coin}/${currency}\"}""")

  override def toOrderIds[C<:Coin, CC<:Currency]: (C,CC) => (OpenOrdersResponse) => immutable.Seq[OrderId[Cryptopia,C,CC]] =
    (coin, currency) => r => r.Data.map(id => OrderId(id.OrderId.map(_.toString).getOrElse("-"), Cryptopia, coin, currency))

  def privateApi(command: String, jsonParam: String): HttpRequest = {

    // api keys
    val apiKey = ApiKeys(this)._1
    val apiSecret = ApiKeys(this)._2

    // signature
    val urlMethod = s"https://www.cryptopia.co.nz/api/$command"
    val nonce = (System.currentTimeMillis + 3000).toString
    val reqSignature = apiKey + "POST" + URLEncoder.encode(urlMethod, "UTF-8").toLowerCase + nonce + Base64.encode(MD5.of(jsonParam.getBytes))
    val auth = "amx " + apiKey + ":" + Base64.encode(Mac.sign(reqSignature, Base64.decode(apiSecret), "HmacSHA256")) + ":" + nonce

    HttpRequest(
      HttpMethods.POST,
      urlMethod,
      entity = HttpEntity(ContentType(MediaTypes.`application/json`), jsonParam),
      headers = RawHeader("Authorization", auth) +: Nil)
  }

  object Response {
    case class OrderBookResponse(Success: Boolean, Message: Option[String], Data: OrderBookData)
    case class OrderBookData(Buy: Vector[OrderBookOrder], Sell: Vector[OrderBookOrder])
    case class OrderBookOrder(Price: Double, Volume: Double)

    implicit val orderBookOrderFormat: RootJsonFormat[OrderBookOrder] = jsonFormat2(OrderBookOrder)
    implicit val orderBookDataFormat: RootJsonFormat[OrderBookData] = jsonFormat2(OrderBookData)

    case class BalancesResponse(Success: Boolean, Error: Option[String], Data: Vector[Balance])
    case class Balance(Symbol: String, Available: Double)

    implicit val balanceFormat: RootJsonFormat[Balance] = jsonFormat2(Balance)
    implicit val balancesDataFormat: RootJsonFormat[Vector[Balance]] = DefaultJsonProtocol.vectorFormat

    case class PlaceOrderResponse(Success: Boolean, Error: Option[String], Data: OrderIdResponse)
    case class OrderIdResponse(OrderId: Option[Long])

    implicit val orderIdFormat: RootJsonFormat[OrderIdResponse] = jsonFormat1(OrderIdResponse)

    case class CancelOrderResponse(Success: Boolean, Error: Option[String])


    case class OpenOrdersResponse(Success: Boolean, Error: Option[String], Data: Vector[OrderIdResponse])

  }

}
