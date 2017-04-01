module Doc (Doc, above, beside, image, text, verbatim) where

import Data.Char (toLower)
import qualified Graphics.GD as GD
import System.FilePath (FilePath, takeExtension)

import IndentsAndLineLengths (Aligner, Font(..), Hyphenator, RichText, getStringSize, getWordSizes, noopHyphenator, raggedRight, renderImage)

-- | A document is a thing which can be rendered to an image.
type Doc = IO GD.Image

-- | Render an image from its path, giving it a border.
image :: FilePath -> Doc
image fp = do
  img <- case map toLower (takeExtension fp) of
    ".gif"  -> GD.loadGifFile  fp
    ".jpeg" -> GD.loadJpegFile fp
    ".jpg"  -> GD.loadJpegFile fp
    _       -> GD.loadPngFile  fp
  (w, h) <- GD.imageSize img
  img' <- GD.newImage (w+2, h+2)
  GD.copyRegion (0, 0) (w, h) img (1, 1) img'
  pure img'

-- | Render aligned text
text :: GD.Color -> GD.Color -> Aligner -> Hyphenator -> Double -> Int -> RichText -> Doc
text bgcol textcol aligner hyphenator fontSize width rtf = do
  sizes <- getWordSizes fontSize rtf
  iota  <- getStringSize fontSize (" ", Normal)
  img <- renderImage bgcol textcol fontSize sizes (aligner hyphenator sizes iota width rtf)
  (w, h) <- GD.imageSize img
  if w /= width
    then do
      img' <- GD.newImage (width, h)
      GD.fillImage bgcol img'
      GD.copyRegion (0, 0) (min width w, h) img (0, 0) img'
      pure img'
    else pure img

-- | Render verbatim monospaced text.
verbatim :: GD.Color -> GD.Color -> Double -> Int -> String -> Doc
verbatim bgcol textcol fontSize width s = do
  let rtf = map (\l -> (l, Monospace)) (lines s)
  sizes <- getWordSizes fontSize rtf
  img <- renderImage bgcol textcol fontSize sizes (raggedRight noopHyphenator sizes 0 0 rtf)
  (w, h) <- GD.imageSize img
  img' <- GD.newImage (width, h)
  GD.fillImage bgcol img'
  GD.copyRegion (0, 0) (min width w, h) img (0, 0) img'
  pure img'

-- | Put one document on top of the other, horizontally centring the
-- smaller.
above :: GD.Color -> Doc -> Doc -> Doc
above bgcol = combine $ \(img1, (w1, h1)) (img2, (w2, h2)) -> do
  let width  = max w1 w2
  let height = h1 + h2
  img <- GD.newImage (width, height)
  GD.fillImage bgcol img
  GD.copyRegion (0, 0) (w1, h1) img1 (if w1 < width then (width - w1) `div` 2 else 0, 0)  img
  GD.copyRegion (0, 0) (w2, h2) img2 (if w2 < width then (width - w2) `div` 2 else 0, h1) img
  pure img

-- | Put one document beside another, vertically centring the smaller.
beside :: GD.Color -> Doc -> Doc -> Doc
beside bgcol = combine $ \(img1, (w1, h1)) (img2, (w2, h2)) -> do
  let width  = w1 + w2
  let height = max h1 h2
  img <- GD.newImage (width, height)
  GD.fillImage bgcol img
  GD.copyRegion (0, 0) (w1, h1) img1 (0,  if h1 < height then (height - h2) `div` 2 else 0) img
  GD.copyRegion (0, 0) (w2, h2) img2 (w1, if h2 < height then (height - h2) `div` 2 else 0) img
  pure img

-- | Helper for combining two documents.
combine :: ((GD.Image, GD.Size) -> (GD.Image, GD.Size) -> IO GD.Image) -> Doc -> Doc -> Doc
combine f doc1 doc2 = do
  img1 <- doc1
  img2 <- doc2
  sz1 <- GD.imageSize img1
  sz2 <- GD.imageSize img2
  f (img1, sz1) (img2, sz2)
