module Common where

import Data.List (inits, nub, tails)
import Data.Maybe (fromMaybe)
import qualified Graphics.GD as GD
import Text.Hyphenation (hyphenate, latin)

-------------------------------------------------------------------------------
-- Text Size

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

-- | Get the size of a word.
wordSize :: [(String, Int)] -> String -> Int
wordSize sizes w = fromMaybe 0 (lookup w sizes)

-------------------------------------------------------------------------------
-- Word Breaking

-- | Every way we can break a word, including no breaks.
fragments :: String -> [String]
fragments s0 = s0 : concatMap (\(h,t) -> [h,t]) (fragments' s0)

-- | Every way we can break a word, not including no breaks.
fragments' :: String -> [(String, String)]
fragments' s0 = (map go . init . tail) (zip (inits s0) (tails s0)) where
  go (h, t) = (h ++ "-", t)

-- | Hyphenate a word with Knuth-Liang
knuthHyphenator :: String -> [(String, String)]
knuthHyphenator w =
  let prefixes = (init . scanl1 (++)) (hyphenate latin w)
  in map (\prefix -> (prefix++"-", drop (length prefix) w)) prefixes


-------------------------------------------------------------------------------
-- Font

fontName :: String
fontName = "/home/barrucadu/projects/justify/font.ttf"

fontSize :: Double
fontSize = 12
