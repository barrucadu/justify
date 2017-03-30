module Main where

import Data.Foldable (for_)

import Common
import qualified Basic
import qualified Indent

main :: IO ()
main = do
  stdin <- getLine
  putStrLn ("Got: '" ++ stdin ++ "'.")
  sizes <- getWordSizes stdin
  mapM_ (\(w,s) -> putStrLn ("'" ++ w ++ "': " ++ show s)) sizes
  iota <- getStringSize " "

  -- basic renderers
  for_ [("1", Basic.justify1), ("2", Basic.justify2), ("3", Basic.justify3), ("4", Basic.justify4), ("5", Basic.justify5), ("6", Basic.justify6)] $ \(n, justifier) ->
    let ls    = justifier width sizes iota (words stdin)
        fname = "out-basic-" ++ n ++ ".png"
    in Basic.render fname width ls

  -- indenting renderers
  for_ [("rr1", Indent.rr1), ("rr2", Indent.rr2), ("rr3", Indent.rr3), ("rl1", Indent.rl1), ("rl2", Indent.rl2), ("justify1", Indent.justify1), ("justify2", Indent.justify2), ("justify3", Indent.justify3)] $ \(n, justifier) ->
    let ls    = justifier width sizes iota (words stdin)
        fname = "out-indent-" ++ n ++ ".png"
    in Indent.render sizes fname ls

-- | Default line length
width :: Int
width = 500
