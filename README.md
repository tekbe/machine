# The Incredible Money Machine

Ever thought of buying crypto coins low at one exchange and sell them higher at another? 
Well, this is it. A machine to even out price rates of coins on different exchanges. Of course, people are doing this already. But on lucky days you cant make 50 cent.

So this is just a fun project to code against the exchange APIs. Maybe it's useful as a starting point to somebody. Currently supported exchanges are Bittrex, Poloniex, Cryptopia and HitBtc. But it can easily be extended (see `Exchange[_]`).

To run the machine you'll need API keys from the exchanges you want to use and put them into the object `money.machine.adapter.ApiKeys` with a method `def apply(e: Exchange[_]): (String, String)` that returns the api and private key for an `Exchange`. And you'll need some coins on the exchanges where you want to trade.

Then, as an example, when you type in the REPL something like 

~~~
MoneyMachine.run(Cryptopia, Poloniex, Bittrex)(XMR, BTC)
~~~

the machine will sell XMR for BTC at one exchange and buy XMR for BTC at another if it's profitable.

~~~
[info] Code Statistics for project:
[info] 
[info] Files
[info] - Total:      12 files
[info] - Scala:      12 files (100,0%)
[info] - Java:       0 files (0,0%)
[info] - Total size: 63.782 Bytes
[info] - Avg size:   5.315 Bytes
[info] - Avg length: 126 lines
[info] 
[info] Lines
[info] - Total:      1.521 lines
[info] - Code:       1.086 lines (71,4%)
[info] - Comment:    98 lines (6,4%)
[info] - Blank:      337 lines (22,2%)
[info] - Bracket:    128 lines (8,4%)
[info] 
[info] Characters
[info] - Total:      57.891 chars
[info] - Code:       54.451 chars (94,1%)
[info] - Comment:    3.440 chars (5,9%)
~~~
