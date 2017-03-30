module BasicJustifiers where

import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Text.Hyphenation (hyphenate, latin)

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
