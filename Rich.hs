module Rich where

import Control.Monad (foldM_)
import Data.List (nub, sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Graphics.GD as GD

import Common hiding (fontName, getWordSizes, getStringSize)

-------------------------------------------------------------------------------
-- Lines and Paragraphs

-- | A paragraph is a collection of lines of text.
type Paragraph = [Line]

-- | A line of text is a non-empty list of spans prefixed with spaces.
type Line = NonEmpty (Int, String, Font)

-- | Get the length of a line.
lineLen :: [((String, Font), Int)] -> Line -> Int
lineLen sizes (w:|ws) = sum [gap + wordSize sizes (s, f) | (gap, s, f) <- w:ws]

-- | Get the length of a line, without considering an initial indent.
lineLenNoIndent :: [((String, Font), Int)] -> Line -> Int
lineLenNoIndent sizes ((_,w,f):|ws) = wordSize sizes (w, f) + sum [gap + wordSize sizes (s, f) | (gap, s, f) <- ws]

-- | Get the number of spans in a line.
lineSpans :: Line -> Int
lineSpans = length

-- | Get the maximum length of a line.
maxLineLen :: [((String, Font), Int)] -> Paragraph -> Int
maxLineLen sizes ls = maximum (0:map (lineLen sizes) ls)

-- | Get the height of a line.
getLineHeight :: IO Int
getLineHeight = do
  ((_,y1), _, (_,y2), _) <- GD.measureString (fontName Normal) fontSize 0 (0, 0) "l" 0
  pure $ round (1.5 * fromIntegral (abs $ y2 - y1))

-- | Render a paragraph of text to a file
render :: [((String, Font), Int)] -> String -> Paragraph -> IO ()
render sizes fname ls0 = GD.savePngFile fname =<< renderImage sizes ls0

-- | Render a paragraph of text to an image.
renderImage :: [((String, Font), Int)] -> Paragraph -> IO GD.Image
renderImage sizes ls0 = do
    lineheight <- getLineHeight
    let width = maxLineLen sizes ls0
    img <- GD.newImage (width, (length ls0 + 1) * lineheight)
    GD.fillImage (GD.rgb 255 255 255) img
    go img lineheight 1 ls0
    pure img
  where
    go img lineheight = goLines where
      goLines n (l:ls) = do
        goLine n l
        goLines (n+1) ls
      goLines _ [] = pure ()

      goLine n ws = do
        let y = n * lineheight
        let r f x s = (\(_, _, (x',_), _) -> x') <$> GD.drawString (fontName f) fontSize 0 (x, y) s 0 img
        foldM_ (\x (xoff, s, f) -> r f (xoff+x) s) 0 ws

-------------------------------------------------------------------------------
-- Fonts

type RichText = [(String, Font)]

-- | Font styles.
data Font = Normal | Bold | Italic | Monospace
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Font file names
fontName :: Font -> String
fontName Normal    = "/home/barrucadu/projects/justify/font.ttf"
fontName Bold      = "/home/barrucadu/projects/justify/bold.ttf"
fontName Italic    = "/home/barrucadu/projects/justify/italic.ttf"
fontName Monospace = "/home/barrucadu/projects/justify/mono.ttf"

-- | Get the size of every word in a string, font-aware.
getWordSizes :: RichText -> IO [((String, Font), Int)]
getWordSizes = fmap (nub . concat) . mapM go where
  go w@(_, Monospace) = do
    size <- getStringSize w
    pure [(w, size)]
  go (s, f) =
    let wsize w = let wf = (w, f) in getStringSize wf >>= \size -> pure (wf, size)
    in mapM wsize (concatMap fragments (words s))

-- | Get the size of a string.
getStringSize :: (String, Font) -> IO Int
getStringSize (s, f) = do
  ((x1,_), _, (x2,_), _) <- GD.measureString (fontName f) fontSize 0 (0, 0) s 0
  pure (x2-x1)

-------------------------------------------------------------------------------
-- Justification

type Justifier = Int -> [((String, Font), Int)] -> Int -> RichText -> Paragraph

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
  raggedLeft = map (\l@((_,w,f):|ws) -> (width0 - lineLen sizes l,w,f):|ws)

-- | Ragged-left with increasing line lengths.
rl2 :: Justifier
rl2 width0 sizes = indentsAndLineLengths lenf raggedLeft width0 sizes where
  lenf n = (0, 50 * (n+1))
  raggedLeft ls =
    let maxWidth = 50 * length ls
    in map (\l@((_,w,f):|ws) -> (maxWidth - lineLen sizes l,w,f):|ws) ls

-- | Justified.
justify1 :: Justifier
justify1 width0 sizes = indentsAndLineLengths lenf justify width0 sizes where
  lenf _  = (0, width0)
  justify = padWords sizes (snd . lenf)

-- | Justified with increasing line lengths
justify2 :: Justifier
justify2 width0 sizes = indentsAndLineLengths lenf justify width0 sizes where
  lenf n  = (0, 50 * (1+n))
  justify = padWords sizes (snd . lenf)

-- | Justified with increasing left indents
justify3 :: Justifier
justify3 width0 sizes = indentsAndLineLengths lenf justify width0 sizes where
  lenf n  = (50 * n, width0)
  justify = padWords sizes (snd . lenf)

-- | Justified with increasing line lengths and left indents
justify4 :: Justifier
justify4 width0 sizes = indentsAndLineLengths lenf justify width0 sizes where
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
indentsAndLineLengths lenf postf _ sizes iota = leastBad iota . map postf . go 0 ([], 0) . concatMap pre where
  -- pre-processing: split up spans into words
  pre w@(_, Monospace) = [w]
  pre (s, f) = [(s', f) | s' <- words s]

  go n ([], _) (w:ws) = go n ([w], wordSize sizes w) ws
  go n (sofar, len) (w:ws)
    | fits n len w = go n (w:sofar, len + iota + wordSize sizes w) ws
    | otherwise =
      (case reverse sofar of
         (word:rest) -> [toLine n word rest : ls | ls <- go (n+1) ([], 0) (w:ws)]
         [] -> [toLine n w [] : ls | ls <- go (n+1) ([], 0) ws])
      ++
      [ toLine n word rest : ls
        | (h,t) <- hyphens w
        , fits n len h
        , let (word:rest) = reverse (h:sofar)
        , ls <- go (n+1) ([], 0) (if null (fst t) then ws else (t:ws))
      ]
  go n (sofar, _) [] = case reverse sofar of
    (word:rest) -> [[toLine n word rest]]
    [] -> [[]]

  fits n len w = len + iota + wordSize sizes w <= snd (lenf n)

  toLine n (w,f) rest = (fst (lenf n), w, f):|[(iota, s, f') | (s, f') <- rest]

  hyphens (w, Monospace) = [((w, Monospace), ("", Monospace))] -- never hyphenate monospaced spans
  hyphens (w, f) = map (\(h,t) -> ((h,f),(t,f))) (knuthHyphenator w)

-- | Put extra padding between words to fill up to the line width.
padWords :: [((String, Font), Int)] -> (Int -> Int) -> Paragraph -> Paragraph
padWords sizes lenf = go 0 where
  go _ [] = []
  go _ [lastLine] = [lastLine]
  go n (l@(_:|[]):ls) = l : go (n+1) ls
  go n (l@(w:|rest):ls) =
    let slack = lenf n - lineLenNoIndent sizes l
        gaps = lineSpans l - 1
        wordSlack = slack `div` gaps
        extraSlack = slack - wordSlack * gaps
        extraSlackPos = 42 `mod` gaps
    in (w:|go' wordSlack extraSlack extraSlackPos rest) : go (n+1) ls

  go' wordSlack extraSlack extraSlackPos ((gap,w,f):ws)
    | extraSlackPos == 0 = (gap + wordSlack + extraSlack, w, f) : go' wordSlack 0 0 ws
    | otherwise = (gap + wordSlack, w, f) : go' wordSlack extraSlack extraSlackPos ws
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
        lbadness (_:|rest) = (maximum (iota : map (\(g,_,_) -> g) rest) - iota) ^ 3
        lhyphenated ((_,w,_):|rest) = last (last (w : map (\(_,w,_) -> w) rest)) == '-'
      in (sum (map lbadness ls), length (filter lhyphenated ls))
