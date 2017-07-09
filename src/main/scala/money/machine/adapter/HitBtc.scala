package money.machine.adapter

import java.time.LocalTime
import java.util.{Locale, UUID}

import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{FormData, HttpMethod, HttpMethods, HttpRequest}
import money.machine.ExchangeBalances.Balances
import money.machine._
import spray.json.DefaultJsonProtocol._
import spray.json.{RootJsonFormat, pimpString}

import scala.collection.immutable
import scala.concurrent.Await
import scala.concurrent.duration.DurationLong
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by tim on 27.05.17.
  */
abstract class HitBtc extends Exchange[HitBtc] {

  import Response._


  override def tokens: immutable.Seq[Token] = List(BTC, XMR, LTC)


  override type OrderBookResponseType = OrderBookResponse
  override val orderBookResponseFormat: RootJsonFormat[OrderBookResponseType] = jsonFormat2(OrderBookResponse)
  override type BalancesResponseType = BalancesResponse
  override val balancesResponseFormat: RootJsonFormat[BalancesResponseType] = jsonFormat1(BalancesResponse)
  override type PlaceOrderResponseType = NewOrderResponse
  override val placeOrderResponseFormat: RootJsonFormat[PlaceOrderResponseType] = jsonFormat1(NewOrderResponse)
  override type CancelOrderResponseType = CancelOrderResponse
  override val cancelOrderResponseFormat: RootJsonFormat[CancelOrderResponseType] = jsonFormat2(CancelOrderResponse)
  override type OpenOrdersResponseType = OpenOrdersResponse
  override val openOrdersResponseFormat: RootJsonFormat[OpenOrdersResponseType] = jsonFormat1(OpenOrdersResponse)

  case class LotAndPriceStep(lot: Double, step: Double)

  // tokens are traded in multiples of trade units ("lots") -> 1 lot is the smallest coin trade volume, e.g. 0.1 LTC
  // prices rate granularity is given in "step", e.g. 0.00001 BTC
  val lotsAndPriceSteps: Map[Coin, Map[Currency, LotAndPriceStep]] = {
    val symbolsResponse = request(HttpRequest(HttpMethods.GET, "https://api.hitbtc.com/api/1/public/symbols"))
      .map(_.parseJson.convertTo[SymbolsResponse])
      .map(_.symbols)

    val symbols: Map[Coin, Map[Currency, Vector[Response.SymbolResponse]]] =
      Await.result(symbolsResponse, 10 seconds)
      .filter(sr =>
        Coin.forName(sr.commodity).isDefined && supports(Coin.forName(sr.commodity).get) &&
        Currency.forName(sr.currency).isDefined && supports(Currency.forName(sr.currency).get))
      .groupBy(s => Coin.forName(s.commodity).get)
      .mapValues(sr => sr.groupBy(c => Currency.forName(c.currency).get))

    // lot size / price step defined for all supported coins
    for {
      c <- coins
      cc <- currencies
    } assert(symbols(c)(cc).size == 1)

    symbols.mapValues(_.mapValues(rs => LotAndPriceStep(rs.head.lot.toDouble, rs.head.step.toDouble)))
  }

  // independent from price rate: lot size
  override def minCoinTradeVolume[C <: Coin, CC <: Currency](priceRate: PriceRate[C, CC]): TradeVolume[C, CC] =
    TradeVolume(lotsAndPriceSteps(priceRate.coin)(priceRate.currency).lot, priceRate)

  override def orderBookRequest: (Coin) => (Currency) => HttpRequest = coin => currency => {
    HttpRequest(
      HttpMethods.GET,
      s"https://api.hitbtc.com/api/1/public/${coin}${currency}/orderbook?format_price=number&format_amount=number")
  }

  override def toOrderBook[C <: Coin, CC <: Currency](coin: C, currency: CC)(r: OrderBookResponse): OrderBook[HitBtc, C, CC] = {
    val now = LocalTime.now()
    OrderBook(
      // lowest ask first
      asks = AskBook(HitBtc, coin, currency,
        offers = r.asks.map(s => Ask(HitBtc, TradeVolume(s._2, PriceRate(coin, s._1, currency)))).sortBy(_.volume.priceRate.rate),
        time = now),
      // highest bid first
      bids = BidBook(HitBtc, coin, currency,
        offers = r.bids.map(b => Bid(HitBtc, TradeVolume(b._2, PriceRate(coin, b._1, currency)))).sortBy(_.volume.priceRate.rate).reverse,
        time = now))
  }

  private def privateApi(method: HttpMethod, command: String, additionalParams: immutable.Seq[(String, String)]): HttpRequest = {

    // api keys
    val apiKey = ApiKeys(this)._1
    val apiSecret = ApiKeys(this)._2

    val nonce = (System.currentTimeMillis + 3000).toString

    val host = "https://api.hitbtc.com"
    val url = s"/api/1/trading/$command?nonce=$nonce&apikey=$apiKey"
    val body = additionalParams.foldLeft("") { case (z, (key, value)) => if (z.isEmpty) s"$key=$value" else s"$z&$key=$value" }

    if (method == HttpMethods.GET) {
      val signature: String = Hex.of(Mac.sign(url + "&" + body, apiSecret.getBytes))
      HttpRequest(
        method = HttpMethods.GET,
        uri = host + url + "&" + body,
        headers = RawHeader("X-Signature", signature) +: Nil)
    } else  {
      val signature: String = Hex.of(Mac.sign(url + body, apiSecret.getBytes))
      HttpRequest(
        method = HttpMethods.POST,
        uri = host + url,
        entity = FormData(additionalParams: _*).toEntity,
        headers = RawHeader("X-Signature", signature) +: Nil)
    }
  }

  override def balancesRequest: HttpRequest = privateApi(HttpMethods.GET, "balance", Nil)

  override def toBalances: (BalancesResponse) => Balances = r => {
    r.balance.map(b => (Token.forName(b.currency_code), BigDecimal(b.cash) - b.reserved))
      .filter(_._1.isDefined)
      .map { case (coin, balance) => (coin.get, balance) }
      .filter { case (coin, _) => supports(coin) }
      .toMap
  }

  override def toOrderId[C <: Coin, CC <: Currency]: (C, CC) => (PlaceOrderResponseType) => OrderId[HitBtc, C, CC] =
    (coin, currency) => r => {
      if (r.ExecutionReport.orderStatus == "rejected") {
        println(s"warn: order was rejected by hitbtc. reason: ${r.ExecutionReport.orderRejectReason.get}")
      }
      OrderId(r.ExecutionReport.clientOrderId, HitBtc, coin, currency)
    }

  override def placeOrderRequest[C <: Coin, CC <: Currency](order: NewOrder[HitBtc, C, CC]): HttpRequest = {

    val quantity = (order.volume.coinVolume / lotsAndPriceSteps(order.coin)(order.currency).lot).toInt

    assert(quantity > 0)

    // price rate as multiple of price step
    val priceRate: Double = (order.priceRate.rate / lotsAndPriceSteps(order.coin)(order.currency).step).toInt * lotsAndPriceSteps(order.coin)(order.currency).step

    // acceptable precision
    assert(priceRate >= lotsAndPriceSteps(order.coin)(order.currency).step * 100)

    val side = order match {
      case BuyOrder(_,_) => "buy"
      case SellOrder(_,_) => "sell"
    }
    privateApi(HttpMethods.POST, "new_order",
      ("clientOrderId", UUID.randomUUID().toString().substring(0, 30)) +:
      ("symbol", order.coin.toString + order.currency.toString) +:
      ("side", side) +:
      ("type", "limit") +:
      ("timeInForce", "GTC") +:
        // -> quantity is coin volume divided by lot size (https://api.hitbtc.com/api/1/public/symbols)
      ("quantity", quantity.toString) +:
      ("price", "%.10f".formatLocal(Locale.US, priceRate)) +:
       Nil)
  }

  override def cancelOrderRequest[C <: Coin, CC <: Currency](orderId: OrderId[HitBtc, C, CC]): HttpRequest =
    privateApi(HttpMethods.POST, "cancel_order", ("clientOrderId", orderId.id) +: Nil)

  override def cancelOrderSuccess: (CancelOrderResponseType) => Boolean = _.ExecutionReport.isDefined

  override def openOrdersRequest[C <: Coin, CC <: Currency](coin: C, currency: CC): HttpRequest =
    privateApi(HttpMethods.GET, "orders/recent", ("max_results", "999") +: ("statuses", "new,partiallyFilled") +: Nil)

  override def toOrderIds[C <: Coin, CC <: Currency]: (C, CC) => (OpenOrdersResponseType) => immutable.Seq[OrderId[HitBtc, C, CC]] =
    (coin, currency) => r => r.orders.map(id => OrderId(id.clientOrderId, HitBtc, coin, currency))


  object Response {

    case class OrderBookResponse(asks: Vector[(Double, Double)], bids: Vector[(Double, Double)])

    implicit val balanceResponseFormat: RootJsonFormat[BalanceResponse] = jsonFormat3(BalanceResponse)

    case class BalanceResponse(currency_code: String, cash: Double, reserved: Double)
    case class BalancesResponse(balance: Vector[BalanceResponse])

    implicit val orderIdResponseFormat: RootJsonFormat[OrderIdResponse] = jsonFormat3(OrderIdResponse)

    case class OrderIdResponse(clientOrderId: String, orderStatus: String, orderRejectReason: Option[String])
    case class NewOrderResponse(ExecutionReport: OrderIdResponse)

    implicit val cancelResultFormat: RootJsonFormat[CancelResult] = jsonFormat1(CancelResult)

    case class CancelResult(clientOrderId: String)
    case class CancelOrderResponse(ExecutionReport: Option[CancelResult], CancelReject: Option[CancelResult])

    case class OpenOrdersResponse(orders: Vector[OrderIdResponse])

    implicit val symbolResponseFormat: RootJsonFormat[SymbolResponse] = jsonFormat4(SymbolResponse)
    implicit val symbolsResponseFormat: RootJsonFormat[SymbolsResponse] = jsonFormat1(SymbolsResponse)

    case class SymbolResponse(commodity: String, currency: String, lot: String, step: String)
    case class SymbolsResponse(symbols: Vector[SymbolResponse])
  }
}
