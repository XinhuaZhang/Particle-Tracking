{-# LANGUAGE RankNTypes #-}
module IO where

import Codec.Picture
import Data.Array.Repa as R
import Data.List as L
import GHC.Float
import Particles
import Text.Printf

-- Extract a pixel at a given position, (x, y, the origin is assumed to be at the corner top left, positive y to the bottom of the image)
read :: FilePath -> IO (R.Array U DIM2 Double)
read filePath = do
  buffer <- readTiff filePath
  case buffer of
    Left msg -> error $ printf "error reading %s\n%s" filePath  msg
    Right dImg ->
      case dImg of
        ImageY8 img ->
          return .
          computeS . fromFunction (Z :. imageHeight img :. imageWidth img) $
          (\(Z :. y :. x) -> fromIntegral $ pixelAt img x y :: Double)
        ImageY16 img ->
          return .
          computeS . fromFunction (Z :. imageHeight img :. imageWidth img) $
          (\(Z :. y :. x) -> fromIntegral $ pixelAt img x y)
        ImageY32 img ->
          return .
          computeS . fromFunction (Z :. imageHeight img :. imageWidth img ) $
          (\(Z :. y :. x) -> fromIntegral $ pixelAt img x y)
        ImageYF img ->
          return .
          computeS . fromFunction (Z :. imageHeight img :. imageWidth img ) $
          (\(Z :. y :. x) -> float2Double $ pixelAt img x y)

write :: FilePath -> R.Array U DIM2 Double -> IO ()
write filePath img = do
  let (Z :. rows :. cols) = extent img
      output =
        ImageY16 $
        generateImage (\x y -> round $ img R.! (Z :. y :. x)) cols rows
  savePngImage filePath output
  
{-# INLINE array2image #-}
array2image :: R.Array U DIM2 Double -> Image Pixel16
array2image arr =
  let (Z :. rows :. cols) = extent arr
   in generateImage (\x y -> round $ arr R.! (Z :. y :. x)) cols rows
