module Main where

import Data.Foldable (for_)

import Common
import qualified Basic

main :: IO ()
main = do
  stdin <- getLine
  putStrLn ("Got: '" ++ stdin ++ "'.")
  sizes <- getWordSizes stdin
  mapM_ (\(w,s) -> putStrLn ("'" ++ w ++ "': " ++ show s)) sizes
  let width = 500
  iota <- getStringSize " "
  for_ [(1, Basic.justify1), (2, Basic.justify2), (3, Basic.justify3), (4, Basic.justify4), (5, Basic.justify5), (6, Basic.justify6)] $ \(n, justifier) ->
    let ls = justifier width sizes iota (words stdin)
        fname = "out-basic" ++ show n ++ ".png"
    in Basic.render fname width ls
