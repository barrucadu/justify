module Main where

import Control.Monad (foldM_)
import Data.List (inits, nub, sortOn, tails)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import qualified Graphics.GD as GD
import Text.Hyphenation (hyphenate, latin)

main :: IO ()
main = do
  stdin <- getLine
  putStrLn ("Got: '" ++ stdin ++ "'.")
  sizes <- getWordSizes stdin
  mapM_ (\(w,s) -> putStrLn ("'" ++ w ++ "': " ++ show s)) sizes
  let width = 500
  iota <- getStringSize " "
  let ls = justify5 width sizes iota (words stdin)
  render "out.png" width ls

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
fragments s0 = s0 : concatMap (\(h,t) -> [h,t]) (fragments' s0)

-- | Every way we can break a word, including no breaks.
fragments' :: String -> [(String, String)]
fragments' s0 = (map go . init . tail) (zip (inits s0) (tails s0)) where
  go (h, t) = (h ++ "-", t)

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

-- | "Web browser text justification": split text into lines with the
-- ragged-right algorithm, then add extra spacing between words.
justify3 :: Justifier
justify3 = padWords justify2

-- | Hyphenation: try to get closer to the line limit by splitting
-- words.
justify4 :: Justifier
justify4 = hyphenated fragments'

-- | Knuth-Liang hyphenation.
justify5 :: Justifier
justify5 = hyphenated go where
  go w =
    let prefixes = (init . scanl1 (++)) (hyphenate latin w)
    in map (\prefix -> (prefix++"-", drop (length prefix) w)) prefixes

-- | Justify text with hyphenation, breaking words with the supplied
-- hyphenator.
hyphenated :: (String -> [(String, String)]) -> Justifier
hyphenated hyphenator = padWords justifier where
  justifier width sizes iota = go ([], 0) where
    go (sofar, len) (w:ws)
      | fits len w = go (w:sofar, len + iota + fromMaybe 0 (lookup w sizes)) ws
      | otherwise = case dropWhile (not . fits len . fst) . sortOn (Down . length . fst) $ hyphenator w of
          ((h,t):_)  -> case reverse (h:sofar) of
            (word:rest) -> toLine word rest : go ([], 0) (t:ws)
            _ -> error "unreachable"
          [] -> case reverse sofar of
            (word:rest) -> toLine word rest : go ([], 0) (w:ws)
            [] -> Line w [] : go ([], 0) ws
    go (sofar, _) [] = case reverse sofar of
      (word:rest) -> [toLine word rest]
      [] -> []

    fits len w = len + iota + fromMaybe 0 (lookup w sizes) <= width

    toLine word rest = Line word [(iota, s) | s <- rest]

-- | Put extra padding between words to fill up to the line width.
padWords :: Justifier -> Justifier
padWords justifier width sizes iota ws0 = go (justifier width sizes iota ws0) where
  go [] = []
  go [lastLine] = [lastLine]
  go (l@(Line w rest):ls) =
    let slack = width - lineLen sizes l
        gaps = lineWords l - 1
        wordSlack = slack `div` gaps
        extraSlack = slack - wordSlack * gaps
        extraSlackPos = 42 `mod` (gaps - 1)
    in Line w (go' wordSlack extraSlack extraSlackPos rest) : go ls

  go' wordSlack extraSlack extraSlackPos ((gap,w):ws)
    | extraSlackPos == 0 = (gap + wordSlack + extraSlack, w) : go' wordSlack 0 0 ws
    | otherwise = (gap + wordSlack, w) : go' wordSlack extraSlack extraSlackPos ws
  go' _ _ _ [] = []

-------------------------------------------------------------------------------
-- Font

fontName :: String
fontName = "/home/barrucadu/projects/justify/font.ttf"

fontSize :: Double
fontSize = 12
