module Common where

import Control.Monad (foldM_)
import Data.List (inits, nub, tails)
import Data.Maybe (fromMaybe)
import qualified Graphics.GD as GD

-------------------------------------------------------------------------------
-- Text Justification

type Justifier = Int -> [(String, Int)] -> Int -> [String] -> [Line]

-------------------------------------------------------------------------------
-- Text

-- | A line of text is a non-empty list of words interspersed with
-- spaces of varying sizes.
data Line = Line String [(Int, String)]

-- | Get the length of a line.
lineLen :: [(String, Int)] -> Line -> Int
lineLen sizes (Line w rest) = wordSize w + sum [gap + wordSize s | (gap, s) <- rest] where
  wordSize s = fromMaybe 0 (lookup s sizes)

-- | Get the number of words in a line.
lineWords :: Line -> Int
lineWords (Line _ rest) = 1 + length rest


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

-- | Every way we can break a word, including no breaks.
fragments :: String -> [String]
fragments s0 = s0 : concatMap (\(h,t) -> [h,t]) (fragments' s0)

-- | Every way we can break a word, not including no breaks.
fragments' :: String -> [(String, String)]
fragments' s0 = (map go . init . tail) (zip (inits s0) (tails s0)) where
  go (h, t) = (h ++ "-", t)


-------------------------------------------------------------------------------
-- Rendering

-- | Render a paragraph of text to an image.
render :: String -> Int -> [Line] -> IO ()
render fname width ls0 = do
    ((_,y1), _, (_,y2), _) <- GD.measureString fontName fontSize 0 (0, 0) "l" 0
    let lineheight = round (1.5 * fromIntegral (abs $ y2 - y1))
    img <- GD.newImage (width, (length ls0 + 1) * lineheight)
    GD.fillImage (GD.rgb 255 255 255) img
    go img lineheight 1 ls0
    GD.savePngFile fname img
  where
    go img lineheight = goLines where
      goLines n (l:ls) = do
        goLine n l
        goLines (n+1) ls
      goLines _ [] = pure ()

      goLine n (Line w rest) = do
        let y = n * lineheight
        let r x s = (\(_, _, (x',_), _) -> x') <$> GD.drawString fontName fontSize 0 (x, y) s 0 img
        foldM_ (\x (xoff, s) -> r (xoff+x) s) 0 ((0,w):rest)


-------------------------------------------------------------------------------
-- Font

fontName :: String
fontName = "/home/barrucadu/projects/justify/font.ttf"

fontSize :: Double
fontSize = 12
