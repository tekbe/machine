package money.machine

import java.security.MessageDigest
import java.time.LocalTime
import java.time.temporal.ChronoUnit
import javax.crypto.spec.SecretKeySpec

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import scala.collection.Seq
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}

object Akka {
  implicit val actorSystem = ActorSystem.create("machine")
  implicit val executor = actorSystem.dispatcher
  implicit val materializer = ActorMaterializer()
}

object Hex {
  def of(buf: Array[Byte]): String = buf.map("%02X" format _).mkString.toLowerCase
}

object Mac {

  def sign(body: String, apiSecret: Array[Byte], algo: String = "HmacSHA512"): Array[Byte] = {
    val mac = javax.crypto.Mac.getInstance(algo)
    mac.init(new SecretKeySpec(apiSecret, algo))
    mac.doFinal(body.getBytes())
  }
}

object Math {

  def min(a: BigDecimal, b: BigDecimal): BigDecimal = if (a <= b) a else b
  def max(a: BigDecimal, b: BigDecimal): BigDecimal = if (min(a, b) == b) a else b

  def min(a: Percent, b: Percent): Percent = if (a <= b) a else b
  def max(a: Percent, b: Percent): Percent = if (min(a, b) == b) a else b

  def min[CC<:Currency](p1: Price[CC], p2: Price[CC]): Price[CC] = if (p1 <= p2) p1 else p2
  def max[CC<:Currency](p1: Price[CC], p2: Price[CC]): Price[CC] = if (min(p1, p2) == p2) p1 else p2

  def min[C<:Coin, CC<:Currency](p1: PriceRate[C, CC], p2: PriceRate[C, CC]): PriceRate[C, CC] = if (p1 <= p2) p1 else p2
  def max[C<:Coin, CC<:Currency](p1: PriceRate[C, CC], p2: PriceRate[C, CC]): PriceRate[C, CC] = if (min(p1, p2) == p2) p1 else p2

  /** @param x value, corresponds to v percent
    * @param y value, corresponds to 100 percent
    * @return v - 100 (positive if x > v) */
  def ratePercent(x: BigDecimal, y: BigDecimal): Percent = if (x == y) Percent(0.0) else Percent(x * 100 / y - 100)

  /** @param x value, corresponds to v percent
    * @param y value, corresponds to 100 percent
    * @return v - 100 (positive if x > v) */
  def ratePercent[C<:Coin, CC<:Currency](x: PriceRate[C,CC], y: PriceRate[C,CC]): Percent = if (x == y) Percent(0.0) else Percent(x.rate * 100 / y.rate - 100)

  /** @param x value, corresponds to v percent
    * @param y value, corresponds to 100 percent
    * @return v - 100 (positive if x > v) */
  def ratePercent[CC<:Currency](x: Price[CC], y: Price[CC]): Percent = if (x == y) Percent(0.0) else Percent(x.amount * 100 / y.amount - 100)

}

object MD5 {
  def of(buf: Array[Byte]): Array[Byte] = MessageDigest.getInstance("MD5").digest(buf)
}

object Base64 {
  def encode(buf: Array[Byte]): String = java.util.Base64.getEncoder.encodeToString(buf)
  def decode(string: String): Array[Byte] = java.util.Base64.getDecoder.decode(string)
}

object Time {

  implicit def toFiniteDuration(d: java.time.Duration): FiniteDuration = Duration.fromNanos(d.toNanos)

  def sameTime(t1: LocalTime, t2: LocalTime, toleranceMs: Long) = math.abs(ChronoUnit.MILLIS.between(t1, t2)) <= toleranceMs
}

object Sorting {

  def isSortedAscending[A<:Ordered[A]](orders: Seq[A]): Boolean =
    if (orders.isEmpty) true
    else if (orders.tail.isEmpty) true
    else orders.head <= orders.tail.head && isSortedAscending(orders.tail)

  def isSortedDescending[A<:Ordered[A]](orders: Seq[A]): Boolean = isSortedAscending(orders.reverse)
}

object Combine {

  def combineFutures[T](futures: Seq[Future[T]]): Future[Seq[T]] =
    if (futures.isEmpty) Future(Nil)
    else futures.head.flatMap(t => combineFutures(futures.tail).map(seq => t +: seq))
}

