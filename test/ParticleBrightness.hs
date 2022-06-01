{-# LANGUAGE BangPatterns #-}
module ParticleBrightness where

import Args
import Control.Monad as M
import IO
import Particles
import Workflow

import Control.Monad.Parallel as MP
import Data.Array.Repa as R
import Data.Vector.Unboxed as VU 
import Data.List as L
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import Text.Printf




main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  opts <- L.foldl (>>=) (return defaultOptions) actions
  let inputFolder = argFolderPath opts
  xs <- listDirectory inputFolder
  let filePaths = L.map (\fileName -> inputFolder </> fileName) xs
  !ys <-
    MP.mapM
      (\filePath -> do
         img <- IO.read filePath
         let threshold = argPixelThreshold opts
             (Z :. rows :. cols) = extent img
             graph = createGraph img threshold
             ps = findParticles graph (-1) rows cols
         return
           ( filePath
           , L.length ps
           , (L.maximum . L.map getBrightness $ ps)
           , (L.minimum . L.map getBrightness $ ps)
           , ((L.sum . L.map getBrightness $ ps) / fromIntegral (L.length ps))))
      filePaths
  -- M.mapM_ (\(a, b, c, d, e) -> printf "%s\n%d %.2f %.2f %.2f\n" a b c d e) ys
  M.mapM_ (\(a, b, c, d, e) -> when (b > 0) (printf "%s %d\n" a b)) ys
