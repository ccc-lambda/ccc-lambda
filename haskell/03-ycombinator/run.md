

```
ghci> :load ycombinator.hs 
[1 of 2] Compiling Main             ( ycombinator.hs, interpreted )

ycombinator.hs:3:19: error:
    • Couldn't match type ‘t0’ with ‘t0 -> a’
      Expected: t0 -> a
        Actual: (t0 -> a) -> a
    • In the first argument of ‘x’, namely ‘x’
      In the first argument of ‘f’, namely ‘(x x)’
      In the expression: f (x x)
    • Relevant bindings include
        x :: (t0 -> a) -> a (bound at ycombinator.hs:3:9)
        f :: a -> a (bound at ycombinator.hs:3:3)
        y :: (a -> a) -> a (bound at ycombinator.hs:3:1)
  |
3 | y f = (\x -> f (x x)) (\x -> f (x x))
  |                   ^

ycombinator.hs:3:35: error:
    • Couldn't match expected type ‘t0’ with actual type ‘t0 -> a’
    • In the first argument of ‘x’, namely ‘x’
      In the first argument of ‘f’, namely ‘(x x)’
      In the expression: f (x x)
    • Relevant bindings include
        x :: t0 -> a (bound at ycombinator.hs:3:25)
        f :: a -> a (bound at ycombinator.hs:3:3)
        y :: (a -> a) -> a (bound at ycombinator.hs:3:1)
  |
3 | y f = (\x -> f (x x)) (\x -> f (x x))
  |                                   ^
Failed, no modules loaded.
```