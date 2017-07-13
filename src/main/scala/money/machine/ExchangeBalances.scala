package money.machine

import Akka._

import java.time.LocalTime
import java.time.temporal.ChronoUnit

import scala.collection.Map
import scala.concurrent.duration.DurationDouble
import scala.util.{Failure, Success}

/**
  * provides balances of registered exchanges (see #withExchanges)
  */
object ExchangeBalances {

  type Balances = Map[Token, BigDecimal]

  val intervalInSec = 8 seconds
  val maxAgeInSec = 25

  private val balances = scala.collection.mutable.Map[Exchange[_], (LocalTime, Balances)]()

  def sufficientBalance[C <: Coin, CC <: Currency](buyOrder: BuyOrder[_, C, CC]): Boolean = {
    val balance = get(buyOrder.exchange, buyOrder.currency)
    if (!balance.isDefined) {
      println(s"warn: balances not defined for ${buyOrder.exchange}")
      true
    } else if (balance.get < buyOrder.volume.price.amount) {
      println(s"warn: insufficient balances for order $buyOrder")
      false
    } else true
  }

  def sufficientBalance[C <: Coin, CC <: Currency](newOrder: NewOrder[_, C, CC]): Boolean = newOrder match {
    case bo@BuyOrder(_,_) => sufficientBalance(bo)
    case so@SellOrder(_,_) => sufficientBalance(so)
  }

  def sufficientBalance[C <: Coin, CC <: Currency](sellOrder: SellOrder[_, C, CC]): Boolean = {
    val balance = get(sellOrder.exchange, sellOrder.coin)
    if (!balance.isDefined) {
      println(s"warn: balances not defined for ${sellOrder.exchange}")
      true
    } else if (balance.get < sellOrder.volume.coinVolume) {
      println(s"warn: insufficient balances for order $sellOrder")
      false
    } else true
  }

  def get(exchange: Exchange[_], token: Token): Option[BigDecimal] = {
    balances synchronized {
      for {
        bs <- balances.get(exchange)
        ageInSec = bs._1.until(LocalTime.now(), ChronoUnit.SECONDS)
        if (ageInSec < maxAgeInSec)
        b <- bs._2.get(token)
      } yield b
    }
  }

  // total coin volume
  def sum(coin: Coin): BigDecimal = {
    balances.keys
      .map(e => get(e, coin).get)
      .fold(BigDecimal(0))((x,y) => x + y)
  }

  // total available sum
  def sum[CC<:Currency](cc: CC): Price[CC] = {
    balances.keys
      .map(e => get(e, cc).get)
      .foldLeft(Price(0, cc))((acc,amount) => Price(acc.amount + amount, cc))
  }

  // available coin + available currency in coin volume
  def sum(coin: Coin, currency: Currency): BigDecimal = {
    sum(coin) + MoneyMachine.averagePriceRate(coin, currency)(balances.keys.toList).coinVolume(sum(currency)).coinVolume
  }

  private def remove(exchange: Exchange[_]) = {
    balances synchronized {
      balances.remove(exchange)
    }
  }

  private def update(exchange: Exchange[_], exchangeBalances: Balances) = {
    balances synchronized {
      balances.put(exchange, (LocalTime.now(), exchangeBalances))
    }
  }

  def withAllExchanges(): Unit = {
    withExchanges(Exchange.exchanges:_*)
  }

  def withExchanges(exchanges: Exchange[_]*): Unit = {
    exchanges.filter(e => balances.get(e).isEmpty).foreach(e => fetch(e))
  }

  private def fetch(exchanges: Exchange[_]*): Unit = {

    def delayed(f: => Unit) = {
      Akka.actorSystem.scheduler.scheduleOnce(delay = intervalInSec)(f)
    }

    exchanges.foreach { e =>
      e.balances.onComplete {
        case Success(b) => update(e, b); delayed(fetch(e))
        case Failure(t) => println(s"exception (balances): ${t.getMessage}"); remove(e); delayed(fetch(e))
      }
    }
  }
}
