module Main where

import Analytic
import Args
import Particles
import States
import Tracks
import Workflow

import Control.Monad.Parallel as MP
import System.Environment
import System.Console.GetOpt
import System.Directory
import System.FilePath

import Data.Array.Repa as R
import Data.List as L
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  print opts
  let inputFolder = argFolderPath opts
      pixelThr = argPixelThreshold opts
      probThr = argProbThreshold opts
      stParm =
        StParm
          (argNumDir opts)
          (argNumSpeed opts)
          (argMeanSpeed opts)
          (argMinSpeed opts)
          (argMaxSpeed opts)
          (argSpeedSTD opts)
      tmParm = TranMatParm (argSigma opts) (argTau opts)
      trParm =
        TrackingParam
          (argPixelThreshold opts)
          (argProbThreshold opts)
          (argMaxNumParticle opts)
  xs <- listDirectory inputFolder
  let firstFileName = inputFolder </> L.head xs
  (rows, cols) <- readRowCol firstFileName
  ps <-
    filterParticles (trackingParamMaximumNumParticle trParm) <$>
    findParticlesFromFile pixelThr firstFileName
  print . L.length $ ps
  let initTracks = createInitTracks stParm ps
  -- tracks <-
  --   findTracksFromFile2 stParm tmParm pixelThr probThr initTracks .
  --   L.map (\x -> inputFolder </> x) . L.take 1 . L.tail $
  --   xs
  let outputFolder = argOutputFolderPath opts
      prParm = PrintParam rows cols outputFolder
  createDirectoryIfMissing True outputFolder
  tracks <-
    findTracksFromFile3 prParm stParm tmParm trParm initTracks .
    L.map (\x -> inputFolder </> x) . L.take 2 . L.tail $
    xs
  MP.mapM_
    (\tr ->
       saveTrack rows cols (outputFolder </> (printf "%d.png" (trackID tr))) tr) .
    L.reverse . L.sortOn (L.length . trackPath) $tracks
