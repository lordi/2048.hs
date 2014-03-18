A version with arbitrary Monoids based on a small barebones haskell cli clone of the [2048](http://git.io/2048) game.

The Monoid needs to have Eq and Show instances.

To change the Monoid, you only need to provide the Monoid member that will be inserted each round, e.g. ``(Sum 2)`` for the original version. Your goal will automatically be determined. For example, for ``(Product 2)`` the game would be called 179769313486231590772930519078902473361797697894230657273430081157732675805500963132708477322407536021120113879871393357658789768814416622492847430639474124377767893424865485276302219601246094119453082952085005768838150682342462881473913110540827237163350510684586298239947245938479716304835356329624224137216.

Of course, your score will also be counted in your Monoid.

```
cabal update && cabal install cabal-install
cabal sandbox init
cabal install --only-dependencies
cabal run 2048
```

* `w`: go up
* `a`: go left
* `s`: go down
* `d`: go right

