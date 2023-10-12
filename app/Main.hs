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
      maxNumberFrame = argMaxNumFrame opts
      pixelThr = argPixelThreshold opts
      probThr = argProbThreshold opts
      stParm =
        StParam
          (argNumDir opts)
          (argNumSpeed opts)
          (argMeanSpeed opts)
          (argMinSpeed opts)
          (argMaxSpeed opts)
          (argSpeedSTD opts)
      tmParm = TranMatParam (argSigma opts) (argTau opts)
      trParm =
        TrackingParam
          (argPixelThreshold opts)
          (argProbThreshold opts)
          (argMaxNumParticle opts)
  xs <- L.drop 0 . L.sort <$> listDirectory inputFolder
  print xs
  let firstFileName = inputFolder </> L.head xs
  (rows, cols) <- readRowCol firstFileName
  ps <-
    filterParticles (trackingParamMaximumNumParticle trParm) <$>
    findParticlesFromFile 0 pixelThr firstFileName
  print . L.length $ ps
  let initTracks = createInitTracks stParm ps
      outputFolder = argOutputFolderPath opts
      prParm = PrintParam rows cols outputFolder
  createDirectoryIfMissing True (outputFolder </> "Images")
  createDirectoryIfMissing True (outputFolder </> "Data")
  tracks <-
    findTracksFromFileCheckpoint prParm stParm tmParm trParm initTracks .
    L.map (\x -> inputFolder </> x) . L.take (maxNumberFrame - 1) . L.tail $
    xs
  let sortedTracks =        
        L.sortOn (snd . particleCenter . L.last . trackPath) .
        removeDuplicateTracks .
        L.filter
          (\tr ->
             ((L.length . trackPath $ tr) >= 5) &&
             (fst . particleCenter . L.last . trackPath $ tr) < 700) $
        tracks
  print . L.map (L.length . trackPath) $ sortedTracks
  MP.mapM_
    (\(idx, tr) ->
       saveTrack
         rows
         cols
         (outputFolder </> "Images" </> (printf "%03d.png" idx))
         tr) .
    L.zip [0 :: Int ..] $
    sortedTracks
  MP.mapM_
    (\(idx, tr) ->
       saveTrackMeanStd (outputFolder </> "Data" </> (printf "%03d.txt" idx)) tr) .
    L.zip [0 :: Int ..] $
    sortedTracks
  saveTracks rows cols (outputFolder </> "Images" </> "sum.png") sortedTracks
