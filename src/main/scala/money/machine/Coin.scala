package money.machine

import java.time.Duration
import java.time.temporal.ChronoUnit

import scala.collection.immutable

// @formatt.er:off
object Token {
  def forName(name: String): Option[Token] = Coin.forName(name).orElse(Currency.forName(name))
}
sealed trait Token

object Coin {
  val coins: immutable.Seq[Coin] = List(DASH, XMR, ZEC, LTC, ARDR, KOBO, XSPEC, HUSH, XZC,
    ETH, XRP, DGB, ETC, STRAT, SC, GNT, DOGE, STR, XEM, BCN, BTS, OMNI, LSK, NXT, REP,
    GAME, STEEM, SYS, FCT, EXP, MAID, DCR, PASC, GNO, BURST, AMP, BLK, XCP, RIC, FLDC, NAUT, BURST, CLAM).distinct

  def forName(name: String): Option[Coin] = coins.find(_.toString == name)
}

sealed trait Coin extends Token {
  val timeDifferenceTolerance: Duration = Duration.of(999, ChronoUnit.MILLIS)
  val latencyTolerance: Duration = Duration.of(1333, ChronoUnit.MILLIS)
  val defaultTradeRateLimitPercent: Percent = Percent(1.0)
  override def toString: String = this.getClass.getSimpleName.split("\\$").last
}

// only BTC so far
object Currency {
  val currencies: immutable.Seq[Currency] = BTC +: Nil
  def forName(name: String): Option[Currency] = currencies.find(_.toString == name)
}
sealed trait Currency extends Token {
  override def toString: String = this.getClass.getSimpleName.split("\\$").last
}


object BTC extends Currency

object LTC extends Coin
object ARDR extends Coin
object KOBO extends Coin
object XSPEC extends Coin
object HUSH extends Coin
object ZEC extends Coin
object XMR extends Coin
object DASH extends Coin
object XZC extends Coin
object ETH extends Coin
object XRP extends Coin
object DGB extends Coin
object ETC extends Coin
object STRAT extends Coin
object SC extends Coin
object GNT extends Coin
object DOGE extends Coin
object STR extends Coin
object XEM extends Coin
object BCN extends Coin
object BTS extends Coin
object OMNI extends Coin
object LSK extends Coin
object NXT extends Coin
object REP extends Coin
object GAME extends Coin
object STEEM extends Coin
object SYS extends Coin
object FCT extends Coin
object EXP extends Coin
object MAID extends Coin
object DCR extends Coin
object PASC extends Coin
object GNO extends Coin
object BURST extends Coin
object AMP extends Coin
object BLK extends Coin
object XCP extends Coin
object RIC extends Coin
object FLDC extends Coin
object NAUT extends Coin
object CLAM extends Coin

