# The Incredible Money Machine

Ever thought of buying crypto coins low at one exchange and sell them higher at another? 
Well, this is it. A machine to even out price rates of coins on different exchanges. Of course, people are doing this already. But on lucky days maybe you cant make 50 cent.

So this is just a fun project to code against the exchange APIs. Maybe it's useful to somebody. Currently supported exchanges are Bittrex, Poloniex, Cryptopia and HitBtc. 

To run the machine you'll need api keys from the exchanges you want to use and put them into the object `money.machine.adatper.ApiKeys` with a method `def apply(e: Exchange[_]): (String, String)` that returns the api and private key for an `Exchange`. And you need some coins on the exchanges you want to trade.

Then you hit something like this 

~~~
MoneyMachine.run(Cryptopia, Poloniex, Bittrex)(XMR, BTC)
~~~

and the machine will try to sell XMR for BTC at one exchange and buy XMR for BTC at another if it's profitable.

~~~
[info] Code Statistics for project:
[info] 
[info] Files
[info] - Total:      12 files
[info] - Scala:      12 files (100,0%)
[info] - Java:       0 files (0,0%)
[info] - Total size: 61.733 Bytes
[info] - Avg size:   5.144 Bytes
[info] - Avg length: 122 lines
[info] 
[info] Lines
[info] - Total:      1.467 lines
[info] - Code:       1.048 lines (71,4%)
[info] - Comment:    94 lines (6,4%)
[info] - Blank:      325 lines (22,2%)
[info] - Bracket:    118 lines (8,0%)
[info] 
[info] Characters
[info] - Total:      56.080 chars
[info] - Code:       52.858 chars (94,3%)
[info] - Comment:    3.222 chars (5,7%)
~~~
