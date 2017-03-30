module Main where

import Data.Foldable (for_)

import Common
import BasicJustifiers

main :: IO ()
main = do
  stdin <- getLine
  putStrLn ("Got: '" ++ stdin ++ "'.")
  sizes <- getWordSizes stdin
  mapM_ (\(w,s) -> putStrLn ("'" ++ w ++ "': " ++ show s)) sizes
  let width = 500
  iota <- getStringSize " "
  for_ [(1, justify1), (2, justify2), (3, justify3), (4, justify4), (5, justify5)] $ \(n, justifier) ->
    let ls = justifier width sizes iota (words stdin)
        fname = "out-" ++ show n ++ ".png"
    in render fname width ls
