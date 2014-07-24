This is an experimental QuasiQuoter for the GHCJS foreign function interface. It allows
you to call JavaScript inline, with named parameters. The QuasiQuoter automatically
splices the top-level foreign imports.

The current version is not yet ready for production, since it uses `unsafePerformIO` and
`fromJust` around the `fromJSRef` and `toJSRef` methods. We might want to modify the classes
to make them more suitable for the purpose of this library.

Example:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Prelude hiding (log)
import GHCJS.Foreign.QQ

log :: String -> IO ()
log msg = [js| console.log(`msg); |]

delay :: Int -> IO ()
delay ms = [jsi| setTimeout($c, `ms); |]

plus :: Int -> Int -> Int
plus x y = [js_| `x + `y |]

main :: IO ()
main = do
  log "hello, world!"
  delay 1000
  print (1 `plus` 2)
```