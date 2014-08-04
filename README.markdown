This is an experimental QuasiQuoter for the GHCJS foreign function interface. It allows
you to call JavaScript inline, with named parameters. The QuasiQuoter automatically
splices the top-level foreign imports.

The current version is not yet ready for production, since it uses experimental
pure marshalling typeclasses.

Example:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Prelude hiding (log)
import GHCJS.Foreign.QQ

log :: String -> IO ()
log msg = [js_| console.log(`msg); |]

delay :: Int -> IO ()
delay ms = [jsi_| setTimeout($c, `ms); |]

plus :: Int -> Int -> Int
plus x y = [js'| `x + `y |]

main :: IO ()
main = do
  log "hello, world!"
  delay 1000
  print (1 `plus` 2)
```

# todo

- make marshalling safer and improve underlying infrastructure
- add a good JavaScript parser with good error reporting
- export a way to let users make custom QuasiQuoters, so they can choose their own marshalling, possibly based on types
- support a combination of named arguments and positional placeholders so that all of these mean the same thing:

```haskell
plus1, plus2, plus3 :: Int -> Int -> Int
plus1 x y = [js'| `x + `y |]
plus2 x   = [js'| `x + $1 |]
plus3     = [js'| $1 + $2 |]
```

