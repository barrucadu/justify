module IndentsAndLineLengths
  ( -- * Document elements
    Paragraph
  , Line
  , Font(..)
  , RichText
  -- * Hyphenation
  , Hyphenator
  , naiveHyphenator
  , noopHyphenator
  , Language(..)
  , knuthHyphenator
  -- * Layout
  , getWordSizes
  , Aligner
  , raggedRight
  , raggedLeft
  , justified
  -- ** Helpers for making your own aligner
  , indentsAndLineLengths
  , padWords
  -- * Rendering
  , render
  , renderImage
  ) where

import Control.Monad (foldM_)
import Data.List (inits, nub, sortOn, tails)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import qualified Graphics.GD as GD
import Text.Hyphenation (hyphenate, languageHyphenator, Language(..))

-------------------------------------------------------------------------------
-- Lines and Paragraphs

-- | A paragraph is a collection of lines of text.
type Paragraph = [Line]

-- | A line of text is a non-empty list of spans prefixed with spaces.
type Line = NonEmpty (Int, String, Font)

-- | Get the length of a line.
lineLen :: [((String, Font), Int)] -> Line -> Int
lineLen sizes (w:|ws) = sum [gap + wordSize sizes (s, f) | (gap, s, f) <- w:ws]

-- | Get the maximum length of a line.
maxLineLen :: [((String, Font), Int)] -> Paragraph -> Int
maxLineLen sizes ls = maximum (0:map (lineLen sizes) ls)

-- | Render a paragraph of text to a file
render :: Double -- ^ The font size
  -> [((String, Font), Int)] -- ^ The word sizes
  -> Paragraph -- ^ The text
  -> String -- ^ The file name
  -> IO ()
render fontSize sizes ls0 fname = GD.savePngFile fname =<< renderImage fontSize sizes ls0

-- | Render a paragraph of text to an image.
renderImage :: Double -- ^ The font size
  -> [((String, Font), Int)] -- ^ The word sizes
  -> Paragraph -- ^ The text
  -> IO GD.Image
renderImage fontSize sizes ls0 = do
    ((_,y1), _, (_,y2), _) <- GD.measureString (fontName Normal) fontSize 0 (0, 0) "l" 0
    let lineheight = round (1.5 * fromIntegral (abs $ y2 - y1) :: Double)
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
getWordSizes :: Double -- ^ The font size
  -> RichText -> IO [((String, Font), Int)]
getWordSizes fontSize = fmap (nub . concat) . mapM go where
  go w@(_, Monospace) = do
    size <- getStringSize fontSize w
    pure [(w, size)]
  go (s, f) =
    let wsize w = let wf = (w, f) in getStringSize fontSize wf >>= \size -> pure (wf, size)
    in mapM wsize (concatMap fragments (words s))

  fragments s0 = s0 : concatMap (\(h,t) -> [h,t]) (naiveHyphenator s0)

-- | Get the size of a string.
getStringSize :: Double -> (String, Font) -> IO Int
getStringSize fontSize (s, f) = do
  ((x1,_), _, (x2,_), _) <- GD.measureString (fontName f) fontSize 0 (0, 0) s 0
  pure (x2-x1)

-- | Get the size of a word.
wordSize :: Eq a => [(a, Int)] -> a -> Int
wordSize sizes w = fromMaybe 0 (lookup w sizes)


-------------------------------------------------------------------------------
-- Hyphenation

-- | A hyphenator takes a word and returns a list of prefix,suffix
-- pairs, with a "-" appended to the prefix.
type Hyphenator = String -> [(String, String)]

-- | Allow breaking a word after any letter.
naiveHyphenator :: Hyphenator
naiveHyphenator w = (map go . init . tail) (zip (inits w) (tails w)) where
  go (h, t) = (h ++ "-", t)

-- | Never hyphenate a word.
noopHyphenator :: Hyphenator
noopHyphenator _ = []

-- | Hyphenate a word with Knuth-Liang
knuthHyphenator :: Language -> Hyphenator
knuthHyphenator lang =
  let hyphenator = hyphenate (languageHyphenator lang)
  in \w ->
    let prefixes = (init . scanl1 (++)) (hyphenator w)
    in map (\prefix -> (prefix++"-", drop (length prefix) w)) prefixes


-------------------------------------------------------------------------------
-- Layout


-- | A text layout algorithm. Takes: the hyphenator to use, the list
-- of word sizes, the width of a space, and the desired line length.
type Aligner = Hyphenator -> [((String, Font), Int)] -> Int -> Int -> RichText -> Paragraph

-- | Ragged-right.
raggedRight :: Aligner
raggedRight hyphenator sizes iota width =
  let lenf _ = (0::Int, width)
      post   = id
  in indentsAndLineLengths lenf post hyphenator sizes iota width

-- | Ragged-left.
raggedLeft :: Aligner
raggedLeft hyphenator sizes iota width =
  let lenf _ = (0::Int, width)
      post   = map (\l@((_,w,f):|ws) -> (width - lineLen sizes l,w,f):|ws)
  in indentsAndLineLengths lenf post hyphenator sizes iota width

-- | Justified.
justified :: Aligner
justified hyphenator sizes iota width =
  let lenf _ = (0::Int, width)
      post   = padWords sizes (snd . lenf)
  in indentsAndLineLengths lenf post hyphenator sizes iota width

-- | Justified text with per-line left indents and lengths.
indentsAndLineLengths
  :: (Int -> (Int, Int))
  -- ^ Argument is the line number (starting from 0). First result is
  -- the left indent, second is the line length.
  -> (Paragraph -> Paragraph)
  -- ^ Post-processing function.
  -> Aligner
indentsAndLineLengths lenf postf hyphenator sizes iota _ = leastBad iota . map postf . go 0 ([], 0) . concatMap pre where
  -- pre-processing: split up spans into words
  pre w@(_, Monospace) = [w]
  pre (s, f) = [(s', f) | s' <- words s]

  go n ([], _) (w:ws)
    -- if this is the first word of the line and neither the word nor any of its hyphenations fit, put it here anyway.
    | not (fits n 0 w) && not (any (fits n 0 . fst) (hyphens w)) = go n ([w], wordSize sizes w) ws
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
        , ls <- go (n+1) ([], 0) (t:ws)
      ]
  go n (sofar, _) [] = case reverse sofar of
    (word:rest) -> [[toLine n word rest]]
    [] -> [[]]

  fits n len w = len + iota + wordSize sizes w <= snd (lenf n)

  toLine n (w,f) rest = (fst (lenf n), w, f):|[(iota, s, f') | (s, f') <- rest]

  hyphens (_, Monospace) = [] -- never hyphenate monospaced spans
  hyphens (w, f) = map (\(h,t) -> ((h,f),(t,f))) (hyphenator w)

-- | Put extra padding between words to fill up to the line width.
padWords :: [((String, Font), Int)] -> (Int -> Int) -> Paragraph -> Paragraph
padWords sizes lenf = go 0 where
  go _ [] = []
  go _ [lastLine] = [lastLine]
  go n (l@(_:|[]):ls) = l : go (n+1) ls
  go n (l@(w:|rest):ls) =
    let slack = lenf n - lineLen sizes l - (let (indent,_,_) = w in indent)
        gaps = length l - 1
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
        lbadness (_:|rest) = (maximum (iota : map (\(g,_,_) -> g) rest) - iota) ^ (3::Int)
        lhyphenated ((_,w,_):|rest) = last (last (w : map (\(_,x,_) -> x) rest)) == '-'
      in (sum (map lbadness ls), length (filter lhyphenated ls))
