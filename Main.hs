module Main where

import Data.Foldable (for_)

import Common
import qualified Basic
import qualified Indent
import qualified Rich
import qualified Shape

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

  -- rich text renderers
  let rtf = makeFancy stdin
  richSizes <- Rich.getWordSizes rtf
  for_ [("rr1", Rich.rr1), ("rr2", Rich.rr2), ("rr3", Rich.rr3), ("rl1", Rich.rl1), ("rl2", Rich.rl2), ("justify1", Rich.justify1), ("justify2", Rich.justify2), ("justify3", Rich.justify3), ("justify4", Rich.justify4)] $ \(n, justifier) ->
    let ls    = justifier width richSizes iota rtf
        fname = "out-rich-" ++ n ++ ".png"
    in Rich.render richSizes fname ls

  -- shape renderers
  let rtf2 = [(stdin, Rich.Normal)]
  richSizes2 <- Rich.getWordSizes rtf2
  for_ [("square", Shape.squareJ, Shape.squareR)] $ \(n, justifier, renderer) ->
    let ls    = justifier width richSizes2 iota rtf2
        fname = "out-shape-" ++ n ++ ".png"
    in renderer richSizes2 fname ls

-- | Default line length
width :: Int
width = 500

-- | Make text fancy
makeFancy :: String -> Rich.RichText
makeFancy = go Rich.Normal . words where
  go _ [] = []
  go f s =
    let (before, after) = (take 3 s, drop 3 s)
        f' = if f == maxBound then minBound else succ f
    in (unwords before, f) : go f' after
