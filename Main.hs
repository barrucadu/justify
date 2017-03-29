module Main where

import Data.List (nub)
import qualified Graphics.GD as GD

main :: IO ()
main = do
  stdin <- getLine
  putStrLn ("Got: '" ++ stdin ++ "'.")
  sizes <- getCharacterSizes (nub stdin)
  mapM_ (\(c,s) -> putStrLn ("'" ++ [c] ++ "': " ++ show s)) sizes

-- | Get the size of every character in a string..
getCharacterSizes :: String -> IO [(Char, Int)]
getCharacterSizes cs0 = GD.withImage (GD.newImage (100, 100)) (go cs0) where
  go (c:cs) img = do
    ((x1,_), _, (x2,_), _) <- GD.measureString fontName fontSize 0  (0, 0) [c] 0
    rest <- go cs img
    pure ((c, x2-x1):rest)
  go [] _ = pure []

fontName :: String
fontName = "/home/barrucadu/projects/justify/font.ttf"

fontSize :: Double
fontSize = 12
