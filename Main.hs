module Main where

import Control.Monad (foldM_)
import Data.List (inits, nub, tails)
import Data.Maybe (fromMaybe)
import qualified Graphics.GD as GD

main :: IO ()
main = do
  stdin <- getLine
  putStrLn ("Got: '" ++ stdin ++ "'.")
  sizes <- getWordSizes stdin
  mapM_ (\(w,s) -> putStrLn ("'" ++ w ++ "': " ++ show s)) sizes
  let width = 500
  iota <- getStringSize " "
  let ls = justify2 width sizes iota (words stdin)
  render "out.png" width ls

-- | A line of text is a non-empty list of words interspersed with
-- spaces of varying sizes.
data Line = Line String [(Int, String)]

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

-------------------------------------------------------------------------------
-- Text Justification

type Justifier = Int -> [(String, Int)] -> Int -> [String] -> [Line]

-- | No wrapping: just put everything on the same line.
justify1 :: Justifier
justify1 _ _ iota (w:ws) = [Line w [(iota, w') | w' <- ws]]
justify1 _ _ _ [] = []

-- | Ragged-right: break when adding a word would exceed the line
-- length.
justify2 :: Justifier
justify2 width sizes iota = go ([], 0) where
  go (sofar, len) (w:ws) =
    let newlen = len + iota + fromMaybe 0 (lookup w sizes)
    in if newlen > width
       then case reverse sofar of
              (word:rest) -> toLine word rest : go ([], 0) (w:ws)
              [] -> Line w [] : go ([], 0) ws
       else go (w:sofar, newlen) ws
  go (sofar, _) [] = case reverse sofar of
    (word:rest) -> [toLine word rest]
    [] -> []

  toLine word rest = Line word [(iota, s) | s <- rest]

-------------------------------------------------------------------------------
-- Font

fontName :: String
fontName = "/home/barrucadu/projects/justify/font.ttf"

fontSize :: Double
fontSize = 12
