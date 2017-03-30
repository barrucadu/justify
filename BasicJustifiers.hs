module BasicJustifiers where

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))

import Common

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
justify5 = hyphenated knuthHyphenator

-- | Least bad: try all (fitting) possibilities when breaking a word
-- and pick the least bad paragraph.
justify6 :: Justifier
justify6 = leastBad justifier where
  justifier width sizes iota = map (padWords' width sizes) . go ([], 0) where
    go ([], _) (w:ws) = go ([w], fromMaybe 0 (lookup w sizes)) ws
    go (sofar, len) (w:ws)
      | fits len w = go (w:sofar, len + iota + fromMaybe 0 (lookup w sizes)) ws
      | otherwise =
        (case reverse sofar of
           (word:rest) -> [toLine word rest : ls | ls <- go ([], 0) (w:ws)]
           [] -> [toLine w [] : ls | ls <- go ([], 0) ws])
        ++
        [ toLine word rest : ls
          | (h,t) <- knuthHyphenator w
          , fits len h
          , let (word:rest) = reverse (h:sofar)
          , ls <- go ([], 0) (t:ws)
        ]
    go (sofar, _) [] = case reverse sofar of
      (word:rest) -> [[toLine word rest]]
      [] -> [[]]

    fits len w = len + iota + fromMaybe 0 (lookup w sizes) <= width

    toLine word rest = Line word [(iota, s) | s <- rest]


-------------------------------------------------------------------------------
-- Helpers

-- | Put extra padding between words to fill up to the line width.
padWords :: Justifier -> Justifier
padWords justifier width sizes iota = padWords' width sizes . justifier width sizes iota

-- | Put extra padding between words to fill up to the line width.
padWords' :: Int -> [(String, Int)] -> [Line] -> [Line]
padWords' width sizes = go where
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


-- | Justify text with hyphenation, breaking words with the supplied
-- hyphenator.
hyphenated :: (String -> [(String, String)]) -> Justifier
hyphenated hyphenator = padWords justifier where
  justifier width sizes iota = go ([], 0) where
    go ([], _) (w:ws) = go ([w], fromMaybe 0 (lookup w sizes)) ws
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

-- | Pick the least-bad paragraph.
leastBad :: (Int -> [(String, Int)] -> Int -> [String] -> [[Line]]) -> Justifier
leastBad justifier width sizes iota ws = case sortOn badness (justifier width sizes iota ws) of
    (leastBad:_) -> leastBad
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
        lbadness (Line _ rest) = (maximum (iota : map fst rest) - iota) ^ 3
        lhyphenated (Line w rest) = last (last (w : map snd rest)) == '-'
      in (sum (map lbadness ls), length (filter lhyphenated ls))
