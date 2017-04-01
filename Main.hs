module Main where

import Data.Foldable (for_)
import Graphics.GD (rgb, savePngFile)

import Common
import qualified Basic
import qualified Indent
import qualified Rich
import qualified Shape
import qualified IndentsAndLineLengths as IAL
import qualified Doc

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
  lineHeight <- Rich.getLineHeight
  for_ [("square", Shape.squareJ, Shape.squareR), ("lain", Shape.lainJ, Shape.lainR)] $ \(n, justifier, renderer) ->
    let ls    = justifier lineHeight width richSizes2 iota rtf2
        fname = "out-shape-" ++ n ++ ".png"
    in renderer richSizes2 fname ls

  savePngFile "doc.png" =<< demoDoc

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

-- | Demo document.
demoDoc :: Doc.Doc
demoDoc =
  let
    -- sizes
    width = 1000
    fontSize = 12
    headingSize = fontSize*3
    smallheadingSize = fontSize*2
    -- colours
    bgcol = rgb 30 30 30
    textcol = rgb 153 153 153
    headingcol = rgb 204 204 204
    verbcol = rgb 131 165 152
    -- document elements
    bigheading s = Doc.text bgcol headingcol IAL.centred IAL.noopHyphenator headingSize width [(s, IAL.Bold)]
    smallheading s = Doc.text bgcol headingcol IAL.centred IAL.noopHyphenator smallheadingSize width [(s, IAL.Bold)]
    imageWithCaption fp s = Doc.image fp `above` Doc.text bgcol textcol IAL.centred IAL.noopHyphenator fontSize width s
    rtf = Doc.text bgcol textcol IAL.justified (IAL.knuthHyphenator IAL.English_GB) fontSize width
    text s = rtf [(s, IAL.Normal)]
    verb = Doc.verbatim bgcol verbcol fontSize width
    -- combinators
    above = Doc.above bgcol
  in foldl above (bigheading "Text Justification Algorithms")
    [ text "I started working on a text justification algorithm last night, as a fun exercise to see how they work. I've intentionally not looked at the Knuth-Plass algorithm. I'm doing this in Haskell. I won't give all the code, but enough to follow what I'm doing, hopefully. Here are some basic types:"
    , verb "\
\-- | A line of text is a non-empty list of words interspersed with spaces of varying size.\n\
\data Line = Line String [(Int, String)]\n\
\\n\
\-- | A text justifier takes a line width, list of word (and word fragment) sizes, a minimum space width,\n\
\-- a list of words, and produces a list of lines\n\
\type Justifier = Int -> [(String, Int)] -> Int -> [String] -> [Line]"
    , imageWithCaption "/home/barrucadu/projects/justify/out-basic-1.png" [("out-1.png:", IAL.Bold), ("a test, to make sure I could actually render things to an image.", IAL.Italic)]
    , verb "\
\justify1 :: Justifier\n\
\justify1 _ _ iota (w:ws) = [Line w [(iota, w') | w' <- ws]]\n\
\justify1 _ _ _ [] = []"
    , imageWithCaption "/home/barrucadu/projects/justify/out-basic-2.png" [("out-2.png:", IAL.Bold), ("a simple greedy ragged-right: put words on a line until they don't fit any more.", IAL.Italic)]
    , verb "\
\justify2 :: Justifier\n\
\justify2 width sizes iota = go ([], 0) where\n\
\  go (sofar, len) (w:ws) =\n\
\    let newlen = len + iota + wordSize sizes w\n\
\    in if newlen > width\n\
\       then case reverse sofar of\n\
\              (word:rest) -> toLine word rest : go ([], 0) (w:ws)\n\
\              [] -> Line w [] : go ([], 0) ws\n\
\       else go (w:sofar, newlen) ws\n\
\  go (sofar, _) [] = case reverse sofar of\n\
\    (word:rest) -> [toLine word rest]\n\
\    [] -> []\n\
\\n\
\  toLine word rest = Line word [(iota, s) | s <- rest]"
    , imageWithCaption "/home/barrucadu/projects/justify/out-basic-3.png" [("out-3.png:", IAL.Bold), ("what I have decided to call \"web browser text justification\".", IAL.Italic)]
    , text "The awful algorithm which makes justification such a no-no on webpages. First, allocate words to lines with ragged-right, then evenly spread spaces to use up the extra space. It can be implemented as a simple modification of the ragged right function:"
    , verb "\
\justify3 :: Justifier\n\
\justify3 = padWords justify2\n\
\\n\
\padWords :: Justifier -> Justifier\n\
\padWords justifier width sizes iota = padWords' width sizes . justifier width sizes iota\n\
\\n\
\padWords' :: Int -> [(String, Int)] -> Paragraph -> Paragraph\n\
\padWords' width sizes = go where\n\
\  go [] = []\n\
\  go [lastLine] = [lastLine]\n\
\  go (l@(Line w rest):ls) =\n\
\    let slack = width - lineLen sizes l\n\
\        gaps = lineWords l - 1\n\
\        wordSlack = slack `div` gaps\n\
\        extraSlack = slack - wordSlack * gaps\n\
\        extraSlackPos = 42 `mod` (gaps - 1)\n\
\    in Line w (go' wordSlack extraSlack extraSlackPos rest) : go ls\n\
\\n\
\  go' wordSlack extraSlack extraSlackPos ((gap,w):ws)\n\
\    | extraSlackPos == 0 = (gap + wordSlack + extraSlack, w) : go' wordSlack 0 0 ws\n\
\    | otherwise = (gap + wordSlack, w) : go' wordSlack extraSlack extraSlackPos ws\n\
\  go' _ _ _ [] = []"
    , text "This is the limit of what we can do without hyphenation."
    , smallheading "Hyphenation"
    , imageWithCaption "/home/barrucadu/projects/justify/out-basic-4.png" [("out-4.png:", IAL.Bold), ("a very naive hyphenation strategy, it just splits words wherever it likes (the fragments' function returns all breakings of a word)", IAL.Italic)]
    , verb "\
\justify4 :: Justifier\n\
\justify4 = hyphenated fragments'\n\
\\n\
\fragments' :: String -> [(String, String)]\n\
\fragments' s0 = (map go . init . tail) (zip (inits s0) (tails s0)) where\n\
\  go (h, t) = (h ++ \"-\", t)\n\
\\n\
\hyphenated :: (String -> [(String, String)]) -> Justifier\n\
\hyphenated hyphenator = padWords justifier where\n\
\  justifier width sizes iota = go ([], 0) where\n\
\    go ([], _) (w:ws) = go ([w], wordSize sizes w) ws\n\
\    go (sofar, len) (w:ws)\n\
\      | fits len w = go (w:sofar, len + iota + wordSize sizes w) ws\n\
\      | otherwise =\n\
\        case dropWhile (not . fits len . fst) . sortOn (Down . length . fst) $ hyphenator w of\n\
\          ((h,t):_)  -> case reverse (h:sofar) of\n\
\            (word:rest) -> toLine word rest : go ([], 0) (t:ws)\n\
\            _ -> error \"unreachable\"\n\
\          [] -> case reverse sofar of\n\
\            (word:rest) -> toLine word rest : go ([], 0) (w:ws)\n\
\            [] -> Line w [] : go ([], 0) ws\n\
\    go (sofar, _) [] = case reverse sofar of\n\
\      (word:rest) -> [toLine word rest]\n\
\      [] -> []\n\
\\n\
\    fits len w = len + iota + wordSize sizes w <= width\n\
\\n\
\    toLine word rest = Line word [(iota, s) | s <- rest]"
    , imageWithCaption "/home/barrucadu/projects/justify/out-basic-5.png" [("out-5.png:", IAL.Bold), ("the Knuth-Liang hyphenation algorithm, here with the Latin rules because I am typesetting Lorem Ipsum.", IAL.Italic)]
    , text "This produces much prettier results."
    , verb "\
\justify5 :: Justifier\n\
\justify5 = hyphenated knuthHyphenator\n\
\\n\
\knuthHyphenator :: String -> [(String, String)]\n\
\knuthHyphenator w =\n\
\  let prefixes = (init . scanl1 (++)) (hyphenate latin w)\n\
\  in map (\\prefix -> (prefix++\"-\", drop (length prefix) w)) prefixes"
    , smallheading "Minimising Badness"
    , text "Until now, all these algorithms have been greedy. Once they commit a word (or a fragment of a word) to a line, that decision is never revisited. I suspect that out-5.png is the limit of what can be achieved with greedy algorithms: it looks good, but not great. This brings me to the next version of the algorithm. When a word doesn't fit on a line in its entirety, all possible hyphenations are tried (including just shoving the word onto the next line entirely). Afterwards, the least-bad paragraph is picked. out-6.png shows the result."
    , imageWithCaption "/home/barrucadu/projects/justify/out-basic-6.png" [("out-6.png:", IAL.Bold), ("non-greedy 'least-bad' hyphenation", IAL.Italic)]
    , verb "\
\justify6 :: Justifier\n\
\justify6 = leastBad justifier where\n\
\  justifier width sizes iota = map (padWords' width sizes) . go ([], 0) where\n\
\    go ([], _) (w:ws) = go ([w], wordSize sizes w) ws\n\
\    go (sofar, len) (w:ws)\n\
\      | fits len w = go (w:sofar, len + iota + wordSize sizes w) ws\n\
\      | otherwise =\n\
\        (case reverse sofar of\n\
\           (word:rest) -> [toLine word rest : ls | ls <- go ([], 0) (w:ws)]\n\
\           [] -> [toLine w [] : ls | ls <- go ([], 0) ws])\n\
\        ++\n\
\        [ toLine word rest : ls\n\
\          | (h,t) <- knuthHyphenator w\n\
\          , fits len h\n\
\          , let (word:rest) = reverse (h:sofar)\n\
\          , ls <- go ([], 0) (t:ws)\n\
\        ]\n\
\    go (sofar, _) [] = case reverse sofar of\n\
\      (word:rest) -> [[toLine word rest]]\n\
\      [] -> [[]]\n\
\\n\
\    fits len w = len + iota + wordSize sizes w <= width\n\
\\n\
\    toLine word rest = Line word [(iota, s) | s <- rest]\n\
\\n\
\leastBad :: (Int -> [(String, Int)] -> Int -> [String] -> [Paragraph]) -> Justifier\n\
\leastBad justifier width sizes iota ws = case sortOn badness (justifier width sizes iota ws) of\n\
\    (least:_) -> least\n\
\    [] -> []\n\
\  where\n\
\    -- the badness of a paragraph is the pair @(sum of line badnesses,\n\
\    -- number of hyphenated lines)@.\n\
\    badness ls =\n\
\      let\n\
\        -- the badness of a line is the difference between the biggest\n\
\        -- space and the smallest possible space, raised to the third\n\
\        -- power. This is nonlinear so that one particularly bad line\n\
\        -- is worse than a few slightly bad lines.\n\
\        lbadness (Line _ rest) = (maximum (iota : map fst rest) - iota) ^ 3\n\
\        lhyphenated (Line w rest) = last (last (w : map snd rest)) == '-'\n\
\      in (sum (map lbadness ls), length (filter lhyphenated ls))"
    , text "I am pretty happy with this, but I suspect there is more I could achieve. I'm not really sure what yet, though."
    , smallheading "Rich Text, Indents, and Line Lengths"
    , imageWithCaption "/home/barrucadu/projects/justify/out-rich-rr3.png" [("out-rich-rr3.png:", IAL.Bold), ("ragged-right", IAL.Italic)]
    , imageWithCaption "/home/barrucadu/projects/justify/out-rich-rl2.png" [("out-rich-rl2.png:", IAL.Bold), ("ragged-left", IAL.Italic)]
    , imageWithCaption "/home/barrucadu/projects/justify/out-rich-justify3.png" [("out-rich-justify3.png:", IAL.Bold), ("justified", IAL.Italic)]
    , text "I've added a couple of new features to my algorithm: rich text (bold, italic, and monospace), variable line-lengths, and variable left-indents. The attached images show some different configurations of the same string, changing formatting every three words. For this, I had to change the line type to include fonts, and allow spaces before even the first word:"
    , verb "type Line = NonEmpty (Int, String, Font)"
    , text "I measure the width of every word and partial word in the input, with the exception of monospaced spans, which never get broken over lines:"
    , verb "\
\type RichText = [(String, Font)]\n\
\\n\
\-- | Font styles.\n\
\data Font = Normal | Bold | Italic | Monospace\n\
\  deriving (Bounded, Enum, Eq, Ord, Read, Show)\n\
\\n\
\-- | Get the size of every word in a string, font-aware.\n\
\getWordSizes :: RichText -> IO [((String, Font), Int)]\n\
\getWordSizes = fmap (nub . concat) . mapM go where\n\
\  go w@(_, Monospace) = do\n\
\    size <- getStringSize w\n\
\    pure [(w, size)]\n\
\  go (s, f) =\n\
\    let wsize w = let wf = (w, f) in getStringSize wf >>= \\size -> pure (wf, size)\n\
\    in mapM wsize (concatMap fragments (words s))\n\
\\n\
\-- | Get the size of a string.\n\
\getStringSize :: (String, Font) -> IO Int\n\
\getStringSize (s, f) = do\n\
\  ((x1,_), _, (x2,_), _) <- GD.measureString (fontName f) fontSize 0 (0, 0) s 0\n\
\  pure (x2-x1)"
    , text "The algorithm isn't actually that much more complex than justify6:"
    , verb "\
\-- | Justified text with per-line left indents and lengths.\n\
\indentsAndLineLengths\n\
\  :: (Int -> (Int, Int))\n\
\  -- ^ Argument is the line number (starting from 0). First result is\n\
\  -- the left indent, second is the line length.\n\
\  -> (Paragraph -> Paragraph)\n\
\  -- ^ Post-processing function.\n\
\  -> Justifier\n\
\indentsAndLineLengths lenf postf _ sizes iota = leastBad iota . map postf . go 0 ([], 0) . concatMap pre where\n\
\  -- pre-processing: split up spans into words\n\
\  pre w@(_, Monospace) = [w]\n\
\  pre (s, f) = [(s', f) | s' <- words s]\n\
\\n\
\  go n ([], _) (w:ws) = go n ([w], wordSize sizes w) ws\n\
\  go n (sofar, len) (w:ws)\n\
\    | fits n len w = go n (w:sofar, len + iota + wordSize sizes w) ws\n\
\    | otherwise =\n\
\      (case reverse sofar of\n\
\         (word:rest) -> [toLine n word rest : ls | ls <- go (n+1) ([], 0) (w:ws)]\n\
\         [] -> [toLine n w [] : ls | ls <- go (n+1) ([], 0) ws])\n\
\      ++\n\
\      [ toLine n word rest : ls\n\
\        | (h,t) <- hyphens w\n\
\        , fits n len h\n\
\        , let (word:rest) = reverse (h:sofar)\n\
\        , ls <- go (n+1) ([], 0) (if null (fst t) then ws else (t:ws))\n\
\      ]\n\
\  go n (sofar, _) [] = case reverse sofar of\n\
\    (word:rest) -> [[toLine n word rest]]\n\
\    [] -> [[]]\n\
\\n\
\  fits n len w = len + iota + wordSize sizes w <= snd (lenf n)\n\
\\n\
\  toLine n (w,f) rest = (fst (lenf n), w, f):|[(iota, s, f') | (s, f') <- rest]\n\
\\n\
\  hyphens (w, Monospace) = [((w, Monospace), (\"\", Monospace))] -- never hyphenate monospaced spans\n\
\  hyphens (w, f) = map (\\(h,t) -> ((h,f),(t,f))) (knuthHyphenator w)"
    , text "This perfoms no justification by itself. The postf parameter can adjust word spacing and line indents to get ragged left or justified text. I don't have any more ideas for how to improve this. I think I might call it done and go read the Knuth-Plass paper to see how close I got."
    , smallheading "Images"
    , imageWithCaption "/home/barrucadu/projects/justify/out-shape-lain.png" [("out-shape-lain.png:", IAL.Bold), ("block images.", IAL.Italic)]
    , text "Block images are fairly simple, given that line indents can be variable. Say you have an image Xpx wide and want a result Ypx wide. You render your text with the first N lines (Y - X - some gap)px wide and the rest Ypx wide. Then the renderer just puts the image in the top-let, offsets the first N lines by (X + some gap) pixels, and renders the remaining lines normally. N will be (height of image / height of line)."
    , verb "\
\lainJ :: Int -> Justifier\n\
\lainJ lineHeight width0 sizes = indentsAndLineLengths lenf justify width0 sizes where\n\
\  justify = padWords sizes (snd . lenf)\n\
\  lenf n\n\
\    | n <= lainImageHeight `div` lineHeight = (lainImageWidth + lainImageGap, width0 - lainImageWidth - lainImageGap)\n\
\    | otherwise = (0, width0)\n\
\\n\
\lainR :: [((String, Font), Int)] -> String -> Paragraph -> IO ()\n\
\lainR sizes fname ls0 = do\n\
\  img <- renderImage sizes ls0\n\
\  lainImg <- GD.loadGifFile lainImageFile\n\
\  GD.copyRegion (0,0) (lainImageWidth,lainImageHeight) lainImg (0,0) img\n\
\  GD.savePngFile fname img\n\
\\n\
\lainImageHeight :: Int\n\
\lainImageHeight = 300\n\
\\n\
\lainImageWidth :: Int\n\
\lainImageWidth = 250\n\
\\n\
\lainImageGap :: Int\n\
\lainImageGap = 15"
    , smallheading "Documents"
    , rtf [("You are reading an example document right now. A small collection of combinators for horizontally and vertically combining", IAL.Normal), ("IO GD.Image", IAL.Monospace), ("values suffices for putting things like this together. But now I have reached the limit of my ideas. I might write a Pandoc renderer for the subset of it that I support, but otherwise I cannot think of any new functionality", IAL.Normal)]
    ]
