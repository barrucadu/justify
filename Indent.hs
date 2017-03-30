module Indent where

import Control.Monad (foldM_)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Graphics.GD as GD

import Common

-------------------------------------------------------------------------------
-- Lines and Paragraphs

-- | A paragraph is a collection of lines of text.
type Paragraph = [Line]

-- | A line of text is a non-empty list of words prefixed with spaces.
type Line = NonEmpty (Int, String)

-- | Get the length of a line.
lineLen :: [(String, Int)] -> Line -> Int
lineLen sizes (w:|ws) = sum [gap + wordSize sizes s | (gap, s) <- w:ws]

-- | Get the length of a line, without considering an initial indent.
lineLenNoIndent :: [(String, Int)] -> Line -> Int
lineLenNoIndent sizes ((_,w):|ws) = wordSize sizes w + sum [gap + wordSize sizes s | (gap, s) <- ws]

-- | Get the number of words in a line.
lineWords :: Line -> Int
lineWords = length

-- | Get the maximum length of a line.
maxLineLen :: [(String, Int)] -> Paragraph -> Int
maxLineLen sizes ls = maximum (0:map (lineLen sizes) ls)

-- | Render a paragraph of text to an image.
render :: [(String, Int)] -> String -> Paragraph -> IO ()
render sizes fname ls0 = do
    let width = maxLineLen sizes ls0
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

      goLine n ws = do
        let y = n * lineheight
        let r x s = (\(_, _, (x',_), _) -> x') <$> GD.drawString fontName fontSize 0 (x, y) s 0 img
        foldM_ (\x (xoff, s) -> r (xoff+x) s) 0 ws

-------------------------------------------------------------------------------
-- Justification

type Justifier = Int -> [(String, Int)] -> Int -> [String] -> Paragraph

-- | Ragged-right: break when adding a word would exceed the line
-- length.
rr1 :: Justifier
rr1 width0 sizes = indentsAndLineLengths lenf raggedRight width0 sizes where
  lenf _ = (0, width0)
  raggedRight = id

-- | Ragged-right with increasing line lengths.
rr2 :: Justifier
rr2 = indentsAndLineLengths lenf raggedRight where
  lenf n = (0, 50 * (1+n))
  raggedRight = id

-- | Ragged-right with increasing line lengths and left indents.
rr3 :: Justifier
rr3 = indentsAndLineLengths lenf raggedRight where
  lenf n = (50 * n, 50 * (1+n))
  raggedRight = id

-- | Ragged-left: break when adding a word would exceed the line
-- length and pad the left edge.
rl1 :: Justifier
rl1 width0 sizes = indentsAndLineLengths lenf raggedLeft width0 sizes where
  lenf _ = (0, width0)
  raggedLeft = map (\l@((_,w):|ws) -> (width0 - lineLen sizes l,w):|ws)

-- | Ragged-left with increasing line lengths.
rl2 :: Justifier
rl2 width0 sizes = indentsAndLineLengths lenf raggedLeft width0 sizes where
  lenf n = (0, 50 * (n+1))
  raggedLeft ls =
    let maxWidth = 50 * length ls
    in map (\l@((_,w):|ws) -> (maxWidth - lineLen sizes l,w):|ws) ls

-- | Justified with increasing line lengths
justify1 :: Justifier
justify1 width0 sizes = indentsAndLineLengths lenf justify width0 sizes where
  lenf n  = (0, 50 * (1+n))
  justify = padWords sizes (snd . lenf)

-- | Justified with increasing left indents
justify2 :: Justifier
justify2 width0 sizes = indentsAndLineLengths lenf justify width0 sizes where
  lenf n  = (50 * n, width0)
  justify = padWords sizes (snd . lenf)

-- | Justified with increasing line lengths and left indents
justify3 :: Justifier
justify3 width0 sizes = indentsAndLineLengths lenf justify width0 sizes where
  lenf n  = (50 * n, 50 * (1+n))
  justify = padWords sizes (snd . lenf)


-------------------------------------------------------------------------------
-- Helpers

-- | Justified text with per-line left indents and lengths.
indentsAndLineLengths
  :: (Int -> (Int, Int))
  -- ^ Argument is the line number (starting from 0). First result is
  -- the left indent, second is the line length.
  -> (Paragraph -> Paragraph)
  -- ^ Post-processing function.
  -> Justifier
indentsAndLineLengths lenf postf width0 sizes iota = leastBad iota . map postf . go 0 ([], 0) where
  go n ([], _) (w:ws) = go n ([w], wordSize sizes w) ws
  go n (sofar, len) (w:ws)
    | fits n len w = go n (w:sofar, len + iota + wordSize sizes w) ws
    | otherwise =
      (case reverse sofar of
         (word:rest) -> [toLine n word rest : ls | ls <- go (n+1) ([], 0) (w:ws)]
         [] -> [toLine n w [] : ls | ls <- go (n+1) ([], 0) ws])
      ++
      [ toLine n word rest : ls
        | (h,t) <- knuthHyphenator w
        , fits n len h
        , let (word:rest) = reverse (h:sofar)
        , ls <- go (n+1) ([], 0) (t:ws)
      ]
  go n (sofar, _) [] = case reverse sofar of
    (word:rest) -> [[toLine n word rest]]
    [] -> [[]]

  fits n len w = len + iota + wordSize sizes w <= snd (lenf n)

  toLine n word rest = (fst (lenf n),word):|[(iota, s) | s <- rest]

-- | Put extra padding between words to fill up to the line width.
padWords :: [(String, Int)] -> (Int -> Int) -> Paragraph -> Paragraph
padWords sizes lenf = go 0 where
  go _ [] = []
  go _ [lastLine] = [lastLine]
  go n (l@(_:|[]):ls) = l : go (n+1) ls
  go n (l@(w:|rest):ls) =
    let slack = lenf n - lineLenNoIndent sizes l
        gaps = lineWords l - 1
        wordSlack = slack `div` gaps
        extraSlack = slack - wordSlack * gaps
        extraSlackPos = 42 `mod` gaps
    in (w:|go' wordSlack extraSlack extraSlackPos rest) : go (n+1) ls

  go' wordSlack extraSlack extraSlackPos ((gap,w):ws)
    | extraSlackPos == 0 = (gap + wordSlack + extraSlack, w) : go' wordSlack 0 0 ws
    | otherwise = (gap + wordSlack, w) : go' wordSlack extraSlack extraSlackPos ws
  go' _ _ _ [] = []

-- | Pick the least-bad paragraph.
leastBad :: Int -> [Paragraph] -> Paragraph
leastBad iota candidates = case sortOn badness candidates of
    (least:_) -> least
    [] -> []
  where
    -- the badness of a paragraph is the pair @(sum of line badnesses,
    -- number of hyphenated lines)@.
    badness ls =
      let
        -- the badness of a line is the difference between the biggest
        -- space and the smallest possible space, raised to the third
        -- power. This is nonlinear so that one particularly bad line
        -- is worse than a few slightly bad lines.
        lbadness (_:|rest) = (maximum (iota : map fst rest) - iota) ^ 3
        lhyphenated ((_,w):|rest) = last (last (w : map snd rest)) == '-'
      in (sum (map lbadness ls), length (filter lhyphenated ls))
