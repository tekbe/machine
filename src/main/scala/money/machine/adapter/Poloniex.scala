package money.machine.adapter

import java.time.LocalTime
import java.util.Locale

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import money.machine.ExchangeBalances.Balances
import money.machine._
import spray.json.DefaultJsonProtocol._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.collection.immutable

// @forma.tter:off
class Poloniex extends Exchange[Poloniex] {

  import Response._

  override val tokens: immutable.Seq[Token] = List(BTC, DASH, XMR, ZEC, LTC, ARDR, ETH, XRP, DGB, BURST, AMP,
    ETC, STRAT, SC, GNT, DOGE, STR, XEM, BCN, BTS, OMNI, LSK, NXT, REP, GAME, STEEM, SYS, FCT, EXP, MAID, DCR, PASC, GNO,
    BLK, XCP, RIC, FLDC, NAUT, BURST, CLAM)

  override type OrderBookResponseType = OrderBookResponse
  override val orderBookResponseFormat: RootJsonFormat[Response.OrderBookResponse] = jsonFormat2(OrderBookResponse)
  override type BalancesResponseType = Map[String, String]
  override val balancesResponseFormat: RootJsonFormat[Map[String, String]] = DefaultJsonProtocol.mapFormat[String, String]
  override type PlaceOrderResponseType = PlaceOrderResponse
  override val placeOrderResponseFormat: RootJsonFormat[Response.PlaceOrderResponse] = jsonFormat1(PlaceOrderResponse)
  override type CancelOrderResponseType = CancelOrderResponse
  override val cancelOrderResponseFormat: RootJsonFormat[CancelOrderResponse] = jsonFormat1(CancelOrderResponse)
  override type OpenOrdersResponseType = Vector[OpenOrder]
  override val openOrdersResponseFormat: RootJsonFormat[OpenOrdersResponseType] = DefaultJsonProtocol.vectorFormat[OpenOrder]

  override def toOrderBook[C <: Coin, CC <: Currency](coin: C, currency: CC)(r: Response.OrderBookResponse): OrderBook[Poloniex, C, CC] = {
    val now = LocalTime.now()
    OrderBook(
      // lowest ask first
      asks = AskBook(Poloniex, coin, currency,
        offers = r.asks.map(s => Ask(this, TradeVolume(s._2, PriceRate(coin, s._1.toDouble, currency)))).sortBy(_.volume.priceRate.rate),
        time = now),
      // highest bid first
      bids = BidBook(Poloniex, coin, currency,
        offers = r.bids.map(b => Bid(this, TradeVolume(b._2, PriceRate(coin, b._1.toDouble, currency)))).sortBy(_.volume.priceRate.rate).reverse,
        time = now))
  }

  def orderBookRequest: Coin => Currency => HttpRequest = coin => currency =>
    HttpRequest(
      HttpMethods.GET,
      s"https://poloniex.com/public?command=returnOrderBook&currencyPair=${currency}_${coin}&depth=100")

  override def toBalances: Map[String, String] => Balances = r =>
    r.map { case (name, value) => (Token.forName(name), value.toDouble) }
      .filter(_._1.isDefined)
      .map { case (coin, balance) => (coin.get, BigDecimal(balance)) }
      .filter { case (coin, _) => supports(coin) }

  override def balancesRequest: HttpRequest = privateApi("returnBalances", Nil)

  override def toOrderId[C <: Coin, CC <: Currency]: (C, CC) => (Response.PlaceOrderResponse) => OrderId[Poloniex, C, CC] =
    (coin, currency) => r => OrderId(r.orderNumber.getOrElse("-"), Poloniex, coin, currency)

  override def placeOrderRequest[C <: Coin, CC <: Currency](order: NewOrder[Poloniex, C, CC]): HttpRequest = {

    val command = order match {
      case BuyOrder(_, _) => "buy"
      case SellOrder(_, _) => "sell"
    }
    privateApi(command, ("currencyPair", s"${order.currency}_${order.coin}") +: ("rate", s"${"%.10f".formatLocal(Locale.US, order.volume.priceRate.rate.doubleValue)}") +: ("amount", s"${"%.10f".formatLocal(Locale.US, order.volume.coinVolume.doubleValue)}") +: Nil)
  }

  override def cancelOrderRequest[C <: Coin, CC <: Currency](orderId: OrderId[Poloniex, C, CC]): HttpRequest =
    privateApi("cancelOrder", ("orderNumber", s"${orderId.id}") +: Nil)

  override def cancelOrderSuccess: (Response.CancelOrderResponse) => Boolean = r => r.success == 1

  override def openOrdersRequest[C <: Coin, CC <: Currency](coin: C, currency: CC): HttpRequest =
    privateApi("returnOpenOrders", ("currencyPair", s"${currency}_${coin}") +: Nil)

  override def toOrderIds[C <: Coin, CC <: Currency]: (C, CC) => (Vector[Response.OpenOrder]) => immutable.Seq[OrderId[Poloniex, C, CC]] =
    (coin, currency) => r => r.map(id => OrderId(id.orderNumber, Poloniex, coin, currency))

  def privateApi(command: String, additionalParams: immutable.Seq[(String, String)]): HttpRequest = {

    // api keys
    val apiKey = ApiKeys(this)._1
    val apiSecret = ApiKeys(this)._2

    // build and sign entity
    val params = ("command", command) +: ("nonce", (System.currentTimeMillis + 3000).toString) +: additionalParams
    val body = params.foldLeft("") { case (z, (key, value)) => if (z.isEmpty) s"$key=$value" else s"$z&$key=$value" }
    val signature: String = Hex.of(Mac.sign(body, apiSecret.getBytes))

    HttpRequest(
      HttpMethods.POST,
      s"https://poloniex.com/tradingApi?",
      entity = FormData(params: _*).toEntity,
      headers = RawHeader("Key", apiKey) +: RawHeader("Sign", signature) +: Nil)
  }


  object Response {

    case class OrderBookResponse(asks: Vector[(String, Double)], bids: Vector[(String, Double)])

    case class PlaceOrderResponse(orderNumber: Option[String])

    case class CancelOrderResponse(success: Int)

    case class OpenOrder(orderNumber: String)

    implicit val openOrderFormat: RootJsonFormat[OpenOrder] = jsonFormat1(OpenOrder)

    case class TickerValueResponse(baseVolume: String,
                                   last: String,
                                   percentChange: String,
                                   high24hr: String,
                                   low24hr: String,
                                   isFrozen: String)

    implicit val tickerValueResponseFormat: RootJsonFormat[TickerValueResponse] = jsonFormat6(TickerValueResponse)
  }

}
