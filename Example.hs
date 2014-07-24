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
