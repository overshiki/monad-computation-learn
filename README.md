

This is how I decompose, analyze and reproduce [`qio-haskell`](https://github.com/alexandersgreen/qio-haskell) as well as [`monad-bayes`](https://github.com/tweag/monad-bayes) packages, trying to figure out the idea behind them.

Those packages share a common strategy to let `Monad` handle [`computational effect`](https://emilyriehl.github.io/files/compose.pdf).
