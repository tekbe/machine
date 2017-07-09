package money.machine

import java.time.LocalTime

import scala.annotation.tailrec
import scala.collection.immutable

// @f.ormatter:off

case class CoinVolumeRange[C<:Coin,CC<:Currency](min: TradeVolume[C,CC], max: TradeVolume[C,CC]) {
  assert(min.coinVolume <= max.coinVolume && min.priceRate == max.priceRate)
  def contains(coinVolume: BigDecimal): Boolean = min.coinVolume <= coinVolume && max.coinVolume >= coinVolume
}

case class PriceRange[CC<:Currency](from: Price[CC], to: Price[CC]) {
  assert(from <= to)
  def toCoinVolumeRange[C<:Coin](priceRate: PriceRate[C,CC]) =
    CoinVolumeRange(priceRate.coinVolume(from), priceRate.coinVolume(to))
}

case class Percent(value: BigDecimal) extends Ordered[Percent] {
  override def toString: String = (if (value<0) Console.RED else Console.GREEN) + "%.2f".format(value) + "%" + Console.RESET
  override def compare(that: Percent): Int = this.value compare that.value
  def of(v2: BigDecimal) = v2 / 100 * value
}

sealed trait Order[E<:Exchange[E], C<:Coin, CC<:Currency] {
  val exchange: Exchange[E]
  val coin: C = volume.priceRate.coin
  val currency: CC = volume.priceRate.currency
  val volume: TradeVolume[C,CC]
  val price = volume.price
  val priceRate = volume.priceRate
}


sealed trait Offer[E<:Exchange[E],C<:Coin,CC<:Currency] extends Order[E,C,CC]

case class Ask[E<:Exchange[E],C<:Coin,CC<:Currency](exchange: Exchange[E], volume: TradeVolume[C,CC]) extends Offer[E,C,CC]
case class Bid[E<:Exchange[E],C<:Coin,CC<:Currency](exchange: Exchange[E], volume: TradeVolume[C,CC]) extends Offer[E,C,CC]

sealed trait NewOrder[E<:Exchange[E],C<:Coin,CC<:Currency] extends Order[E,C,CC]

case class BuyOrder[E<:Exchange[E],C<:Coin,CC<:Currency](exchange: Exchange[E], volume: TradeVolume[C,CC]) extends NewOrder[E,C,CC]
case class SellOrder[E<:Exchange[E],C<:Coin,CC<:Currency](exchange: Exchange[E], volume: TradeVolume[C,CC]) extends NewOrder[E,C,CC]

case class OrderId[E<:Exchange[E], C<:Coin, CC<:Currency](id: String, exchange: Exchange[E], coin: C, currency: CC)

/** amount of currency CC */
case class Price[CC<:Currency](amount: BigDecimal, currency: CC) extends Ordered[Price[CC]] {
  def +(that: Price[CC]) = Price(this.amount + that.amount, currency)
  def -(that: Price[CC]) = Price(this.amount - that.amount, currency)
  override def compare(that: Price[CC]): Int = this.amount compare that.amount
}

/** price ratio between coin C and currency CC: how much of CC for one C */
case class PriceRate[C<:Coin, CC<:Currency](coin: C, rate: BigDecimal, currency: CC) extends Ordered[PriceRate[C,CC]] {
  assert(rate > 0)
  def coinVolume(price: Price[CC]) = TradeVolume(price.amount / rate, this)
  override def compare(that: PriceRate[C, CC]): Int = this.rate compare that.rate
}

case class TradeVolume[C<:Coin, CC<:Currency](coinVolume: BigDecimal, priceRate: PriceRate[C,CC]) {
  def take(price: Price[CC]): TradeVolume[C,CC] = TradeVolume(Math.min(this.coinVolume, price.amount / priceRate.rate), priceRate)
  def take(volume: BigDecimal): TradeVolume[C, CC] = TradeVolume(Math.min(this.coinVolume, volume), priceRate)
  def price: Price[CC] = Price(coinVolume * priceRate.rate, priceRate.currency)
}

trait Book[+O <: Offer[E, C, CC], E<:Exchange[E], C <: Coin, CC <: Currency] {

  val offers: immutable.Seq[O]
  val time: LocalTime

  val exchange: Exchange[E]
  val coin: C
  val currency: CC

  def size: Int = offers.size
  def isEmpty: Boolean = offers.isEmpty
  def head: O = offers.head
  def tail: Book[O, E, C, CC]
  /** available coin volume at given price rate */
  def coinVolume(priceRate: PriceRate[C,CC]): BigDecimal
  /** total number of coins in order book */
  def coinsAvailable(): BigDecimal = offers.foldLeft[BigDecimal](0)((acc, o) => acc + o.volume.coinVolume)
  /** total price of coins in order book */
  def priceAvailable(): Price[CC] = offers.foldLeft(Price(0, currency))((acc, o) => acc + o.price)
  /** price for coinVolume */
  def price(coinVolume: BigDecimal): Price[CC] = {
    assert(coinVolume <= this.coinsAvailable)
    if (this.isEmpty || coinVolume <= 0) Price(0, currency)
    else head.volume.take(coinVolume).price + tail.price(coinVolume - head.volume.coinVolume)
  }
  /** price rate at which coinVolume is realizable */
  @tailrec
  final def priceRate(coinVolume: BigDecimal): PriceRate[C,CC] = {
    assert(coinVolume <= this.coinsAvailable)
    if (head.volume.coinVolume >= coinVolume) head.priceRate
    else tail.priceRate(coinVolume - head.volume.coinVolume)
  }
  @tailrec
  final def priceRate(price: Price[CC]): PriceRate[C,CC] = {
    assert(price <= this.priceAvailable)
    if (head.price >= price) head.priceRate
    else tail.priceRate(price - head.price)
  }
  def coinVolume(price: Price[CC]): BigDecimal = {
    assert(price <= this.priceAvailable)
    if (head.price >= price) head.volume.take(price).coinVolume
    else head.volume.coinVolume + tail.coinVolume(price - head.price)
  }

  def createOrder(price: Price[CC]): NewOrder[E,C,CC]
  def createOrder(coinVolume: BigDecimal): NewOrder[E,C,CC]

  /** ask/bid books are roughly from the same point in time */
  def sameTime(that: Book[_, _, C, CC]): Boolean =
    Time.sameTime(this.time, that.time, coin.timeDifferenceTolerance.toMillis)
}

case class AskBook[E<:Exchange[E], C <: Coin, CC <: Currency](exchange: Exchange[E], coin: C, currency: CC,
                                                                offers: immutable.Seq[Ask[E, C, CC]],
                                                                time: LocalTime = LocalTime.now()) extends Book[Ask[E, C, CC], E, C, CC] {
  override def tail = AskBook(exchange, coin, currency, offers.tail, time)
  assert(Sorting.isSortedAscending(offers.map(_.volume.priceRate)))
  override def createOrder(coinVolume: BigDecimal): BuyOrder[E, C, CC] = {
    assert(coinVolume > 0)
    assert(coinVolume <= this.coinsAvailable)
    BuyOrder(exchange, TradeVolume(coinVolume, priceRate(coinVolume)))
  }

  override def createOrder(price: Price[CC]): BuyOrder[E, C, CC] = {
    assert(price > Price(0, currency))
    assert(price <= this.priceAvailable())
    BuyOrder(exchange, TradeVolume(coinVolume(price), priceRate(price)))
  }
  override def coinVolume(priceRate: PriceRate[C, CC]): BigDecimal = {
    if (this.isEmpty || this.head.priceRate > priceRate) 0
    else this.head.volume.coinVolume + this.tail.coinVolume(priceRate)
  }
}

case class BidBook[E<:Exchange[E], C <: Coin, CC <: Currency](exchange: Exchange[E], coin: C, currency: CC,
                                                                offers: immutable.Seq[Bid[E, C, CC]],
                                                                time: LocalTime = LocalTime.now()) extends Book[Bid[E, C, CC], E, C, CC] {
  override def tail = BidBook(exchange, coin, currency, offers.tail, time)
  assert(Sorting.isSortedDescending(offers.map(_.volume.priceRate)))
  override def createOrder(coinVolume: BigDecimal): SellOrder[E, C, CC] = {
    assert(coinVolume > 0)
    assert(coinVolume <= this.coinsAvailable)
    SellOrder(exchange, TradeVolume(coinVolume, priceRate(coinVolume)))
  }
  override def createOrder(price: Price[CC]): NewOrder[E, C, CC] = {
    assert(price > Price(0, currency))
    assert(price <= this.priceAvailable())
    SellOrder(exchange, TradeVolume(coinVolume(price), priceRate(price)))
  }
  override def coinVolume(priceRate: PriceRate[C, CC]): BigDecimal = {
    if (this.isEmpty || this.head.priceRate < priceRate) 0
    else this.head.volume.coinVolume + this.tail.coinVolume(priceRate)
  }
}


/** highest bids/existing buy orders, in decreasing order
  * lowest asks/existing sell orders, in increasing order */
case class OrderBook[E<:Exchange[E],C<:Coin, CC<:Currency](asks: AskBook[E,C,CC], bids: BidBook[E,C,CC]) {

  val exchange: Exchange[E] = asks.exchange
  val coin: C = asks.coin
  val currency: CC = asks.currency
  val time: LocalTime = asks.time

  assert(!asks.isEmpty)
  assert(!asks.isEmpty)
  assert(asks.sameTime(bids))

  /** order books are roughly from the same point in time */
  def sameTime(that: OrderBook[_, C, CC]): Boolean =
    Time.sameTime(this.time, that.time, coin.timeDifferenceTolerance.toMillis)

}