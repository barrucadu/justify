module Shape where

import Control.Monad (foldM_, mapM_)
import qualified Graphics.GD as GD

import Common (fontSize)
import Rich (Font(..), Justifier, Paragraph, fontName, indentsAndLineLengths, lineLen, maxLineLen, padWords, renderImage)

-------------------------------------------------------------------------------
-- Square

-- | Justify text in a way which can be rendered into a square by
-- 'squareR'.
squareJ :: a -> Justifier
squareJ _ width0 sizes = indentsAndLineLengths lenf justify width0 sizes where
  justify = padWords sizes (snd . lenf)
  lenf n
    | n < squareGapStart || n >= (squareGapStart + squareGapLines*2) = (0, width0)
    | otherwise = (0, (width0 - squareGapSize width0) `div` 2)

-- | Render text with a square gap. Assumes text has been justified with 'squareJ'.
squareR :: [((String, Font), Int)] -> String -> Paragraph -> IO ()
squareR sizes fname ls0 = do
    let width = maxLineLen sizes ls0
    ((_,y1), _, (_,y2), _) <- GD.measureString (fontName Normal) fontSize 0 (0, 0) "l" 0
    let lineheight = round (1.5 * fromIntegral (abs $ y2 - y1))
    img <- GD.newImage (width, (countLines 0 ls0 + 1) * lineheight)
    GD.fillImage (GD.rgb 255 255 255) img
    go (width `div` 4 + squareGapSize width) img lineheight 0 ls0
    GD.savePngFile fname img
  where
    go gapxoff img lineheight = goLines lineheight where
      goLines yoff n ls0@(l:ls)
        | n == squareGapStart = goLines' yoff 0 ls0
        | otherwise = drawLineAt yoff 0 l >> goLines (yoff+lineheight) (n+1) ls
      goLines _ _ [] = pure ()
      goLines' yoff n ls0@(l1:l2:ls)
        | n == squareGapLines = goLines'' yoff ls0
        | otherwise = drawLineAt yoff 0 l1 >> drawLineAt yoff gapxoff l2 >> goLines' (yoff+lineheight) (n+1) ls
      goLines' yoff _ [l] = drawLineAt yoff 0 l
      goLines' _ _ [] = pure ()
      goLines'' yoff (l:ls) = drawLineAt yoff 0 l >> goLines'' (yoff+lineheight) ls
      goLines'' _ [] = pure ()

      drawLineAt y0 x0 ws = do
        let r f x s = (\(_, _, (x',_), _) -> x') <$> GD.drawString (fontName f) fontSize 0 (x+x0, y0) s 0 img
        foldM_ (\x (xoff, s, f) -> r f (xoff+x) s) 0 ws

    countLines n (_:ls)
      | n == squareGapStart = 1 + countLines' 1 ls
      | otherwise = 1 + countLines (n+1) ls
    countLines _ [] = 0
    countLines' n (_:ls)
      | n == squareGapLines*2 = length ls
      -- every other gapped-line is typeset on the same line as the prior one
      | n `mod` 2 == 0 = countLines' (n+1) ls
      | otherwise = 1 + countLines' (n+1) ls
    countLines' _ [] = 0

-- | Starting line for the square gap.
squareGapStart :: Int
squareGapStart = 5

-- | Number of lines in the gap.
squareGapLines :: Int
squareGapLines = 6

-- | Size of the gap, as a function of line width.
squareGapSize :: Int -> Int
squareGapSize = (`div`2)


-------------------------------------------------------------------------------
-- Lain

-- | Justify text to flow around a lain image.
lainJ :: Int -> Justifier
lainJ lineHeight width0 sizes = indentsAndLineLengths lenf justify width0 sizes where
  justify = padWords sizes (snd . lenf)
  lenf n
    | n <= lainImageHeight `div` lineHeight = (lainImageWidth + lainImageGap, width0 - lainImageWidth - lainImageGap)
    | otherwise = (0, width0)

-- | Render text + lain image.
lainR :: [((String, Font), Int)] -> String -> Paragraph -> IO ()
lainR sizes fname ls0 = do
  img <- renderImage sizes ls0
  lainImg <- GD.loadGifFile lainImageFile
  GD.copyRegion (0,0) (lainImageWidth,lainImageHeight) lainImg (0,0) img
  GD.savePngFile fname img

-- | Pixel height of the lain image.
lainImageHeight :: Int
lainImageHeight = 300

-- | Pixel width of the lain image.
lainImageWidth :: Int
lainImageWidth = 250

-- | Pixel size of horizontal gap between lain image and text
lainImageGap :: Int
lainImageGap = 15

-- | Filename of the lain image.
lainImageFile :: String
lainImageFile = "/home/barrucadu/projects/justify/lain.gif"
