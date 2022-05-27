module Plot where

import Particles
import States

import Data.Array.Repa as R
import Data.List as L
import Graphics.Gnuplot.Simple


plotDirection :: FilePath -> Particle -> IO ()
plotDirection filePath p = do
  let st = particleState p
      (Z :. numDir :. _) = R.extent st
      center = div numDir 2
      xs = R.toList . R.sumS $ st
      ys = (L.drop center xs) L.++ (L.take center xs)
      deltaTheta = 2 * pi / fromIntegral numDir
      zs = L.map (\i -> fromIntegral (i - center) * deltaTheta) [0 .. numDir - 1]
  plotPath [PNG filePath] $ L.zip zs ys
