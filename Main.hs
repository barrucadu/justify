module Main where

import Data.List (inits, nub, tails)
import qualified Graphics.GD as GD

main :: IO ()
main = do
  stdin <- getLine
  putStrLn ("Got: '" ++ stdin ++ "'.")
  sizes <- getWordSizes stdin
  mapM_ (\(w,s) -> putStrLn ("'" ++ w ++ "': " ++ show s)) sizes

-- | Get the size of every word in a string.
--
-- It's better to operate on whole words rather than characters as
-- this will take into account ligatures, which may result in a
-- rendered string being narrower than just the sum of its character
-- widths.
getWordSizes :: String -> IO [(String, Int)]
getWordSizes = mapM go . nub . concatMap fragments . words where
  go w = do
    size <- getStringSize w
    pure (w, size)

-- | Get the size of a string.
getStringSize :: String -> IO Int
getStringSize str = do
  ((x1,_), _, (x2,_), _) <- GD.measureString fontName fontSize 0 (0, 0) str 0
  pure (x2-x1)

-- | Every way we can break a word, including no breaks.
fragments :: String -> [String]
fragments s0 = s0 : concatMap go (zip (inits s0) (tails s0)) where
  go ([], _) = []
  go (_, []) = []
  go (h, t) = [h ++ "-", '-' : t]

fontName :: String
fontName = "/home/barrucadu/projects/justify/font.ttf"

fontSize :: Double
fontSize = 12
