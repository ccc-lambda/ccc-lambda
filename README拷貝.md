

## install

https://www.haskell.org/ghcup/#

    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

## run

```
(base) cccimac@cccimacdeiMac 01-factorial % ghci
GHCi, version 9.4.8: https://www.haskell.org/ghc/  :? for help

ghci> :load factorial.hs
[1 of 2] Compiling Main             ( factorial.hs, interpreted )
Ok, one module loaded.
ghci> factorial 5
120
```
