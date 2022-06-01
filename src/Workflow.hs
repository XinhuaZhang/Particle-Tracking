{-# LANGUAGE BangPatterns #-}
module Workflow where

import Analytic
import Args
import IO
import Particles
import States
import Tracks


import Control.DeepSeq
import Control.Monad as M
import Control.Monad.Parallel as MP
import Control.Parallel.Strategies
import Data.Array.Repa as R
import Data.List as L
import Data.Vector.Unboxed as VU
import Text.Printf

import System.Directory
import System.FilePath

import Debug.Trace


data TrackingParam = TrackingParam
  { trackingParamPixelThreshold :: Int
  , trackingParamProbThreshold :: Double
  , trackingParamMaximumNumParticle :: Int
  } deriving (Show)
  
data PrintParam = PrintParam
  { printParamRows :: Int
  , printParamCols :: Int
  , printParamFilePath :: FilePath
  } deriving (Show)

{-# inline readRowCol #-}
readRowCol :: FilePath -> IO (Int,Int)
readRowCol filePath = do
  img <- IO.read filePath
  let (Z :. rows :. cols) = extent img
  return (rows, cols)

{-# INLINE findParticlesFromFile #-}
findParticlesFromFile :: Int -> Int -> FilePath  -> IO [Particle]
findParticlesFromFile frameIdx thr filePath = do
  printCurrentTime filePath
  img <- IO.read filePath
  let (Z :. rows :. cols) = extent img
      graph = createGraph img thr
      ps = findParticles graph frameIdx rows cols
  return ps
  
{-# INLINE createInitTracks #-}
createInitTracks :: StParam -> [Particle] -> [Track]
createInitTracks stParm = initTracks . L.map (setState st) . L.reverse
  where
    st = initState stParm

{-# INLINE findTracksFromFile2 #-}
findTracksFromFile2 ::
     StParam -> TranMatParam -> Int -> Double -> [Track] -> [FilePath] -> IO [Track]
findTracksFromFile2 stParm tmParm pixelThr probThr tracks filePaths = do
  let currentMaxTrackLen = L.maximum . L.map (L.length . trackPath) $ tracks
  (results, _, _) <-
    M.foldM
      (\(tr, maxLen, iteration) filePath -> do
         ps <- findParticlesFromFile (-1) pixelThr filePath
         printCurrentTime . show . L.length $ ps
         newTr <-
           M.foldM (updateTracks2 iteration stParm tmParm probThr maxLen) tr ps
         printCurrentTime . show . L.length $ newTr
         -- M.mapM_ print newTr
         return (newTr, maxLen + 1, iteration + 1))
      (tracks, currentMaxTrackLen, 0)
      filePaths
  return results



{-# INLINE findTracksFromFile1 #-}
findTracksFromFile1 ::
     StParam -> TranMatParam -> Int -> [Track] -> [FilePath] -> IO [Track]
findTracksFromFile1 stParm tmParm thr tracks filePaths =
  M.foldM
    (\tr filePath -> do
       printCurrentTime filePath
       ps <- findParticlesFromFile (-1) thr filePath
       printCurrentTime . show . L.length $ ps
       return $!! L.foldl' (updateTracks1 stParm tmParm thr) tr ps)
    tracks
    filePaths
    

{-# INLINE findTracksFromFile3 #-}
findTracksFromFile3 ::
     PrintParam
  -> StParam
  -> TranMatParam
  -> TrackingParam
  -> [Track]
  -> [FilePath]
  -> IO [Track]
findTracksFromFile3 prParam stParm tmParm trParam tracks filePaths = do
  let pixelThr = trackingParamPixelThreshold trParam
      probThr = trackingParamProbThreshold trParam
      folderPath = printParamFilePath prParam
      rows = printParamRows prParam
      cols = printParamCols prParam
      currentMaxTrackLen = L.maximum . L.map (L.length . trackPath) $ tracks
  (results, _, _) <-
    M.foldM
      (\(tr, maxLen, iteration) filePath -> do
         ps <-
           filterParticles (trackingParamMaximumNumParticle trParam) <$>
           findParticlesFromFile iteration pixelThr filePath
         printCurrentTime . show . L.length $ ps
         newTr <-
           trimTracks iteration <$>
           M.foldM
             (updateTracks3
                folderPath
                rows
                cols
                iteration
                stParm
                tmParm
                probThr
                maxLen)
             tr
             ps
         printCurrentTime . show . L.length $ newTr
         -- M.mapM_ print newTr
         return (newTr, maxLen + 1, iteration + 1))
      (tracks, currentMaxTrackLen, 1)
      filePaths
  return results


{-# INLINE findTracksFromFile4 #-}
findTracksFromFile4 ::
     PrintParam
  -> StParam
  -> TranMatParam
  -> TrackingParam
  -> [Track]
  -> [FilePath]
  -> IO [Track]
findTracksFromFile4 prParam stParm tmParm trParam tracks filePaths = do
  let pixelThr = trackingParamPixelThreshold trParam
      probThr = trackingParamProbThreshold trParam
      folderPath = printParamFilePath prParam
      rows = printParamRows prParam
      cols = printParamCols prParam
      currentMaxTrackLen = L.maximum . L.map (L.length . trackPath) $ tracks
  (results, _) <-
    M.foldM
      (\(trs, frameIdx) filePath -> do
         ps <-
           filterParticles (trackingParamMaximumNumParticle trParam) <$>
           findParticlesFromFile frameIdx pixelThr filePath
         printCurrentTime (printf "%d particles found." (L.length ps))
         -- print ps
         newTrs' <-
           M.mapM
             (updateTrackParticlesDebug
                folderPath
                rows
                cols
                frameIdx
                stParm
                tmParm
                probThr
                ps)
             trs
         -- print newTrs'
         let trimmedNewTrs' = trimTracks frameIdx newTrs'
         printCurrentTime
           (printf
              "%d tracks removed."
              (L.length newTrs' - L.length trimmedNewTrs'))
         let unusedParticles = findUnusedParticles frameIdx trimmedNewTrs' ps
             newTrs =
               (createTracksFromParticles
                  (trackID . L.head $ newTrs')
                  frameIdx
                  stParm
                  unusedParticles) L.++
               newTrs'
         -- print . L.length $ unusedParticles
         -- print unusedParticles
         -- printCurrentTime . show . L.length $ newTrs
         return (newTrs, frameIdx + 1))
      (tracks, 1)
      filePaths
  return results
  

{-# INLINE findTracksFromFile5 #-}
findTracksFromFile5 ::
     PrintParam
  -> StParam
  -> TranMatParam
  -> TrackingParam
  -> [Track]
  -> [FilePath]
  -> IO [Track]
findTracksFromFile5 prParam stParm tmParm trParam tracks filePaths = do
  let !pixelThr = trackingParamPixelThreshold trParam
      !probThr = trackingParamProbThreshold trParam
      !folderPath = printParamFilePath prParam
      !rows = printParamRows prParam
      !cols = printParamCols prParam
  (results, _) <-
    M.foldM
      (\(trs, frameIdx) filePath -> do
         ps <-
           filterParticles (trackingParamMaximumNumParticle trParam) <$>
           findParticlesFromFile frameIdx pixelThr filePath
         printCurrentTime (printf "%d particles found." (L.length ps))
         -- print ps
         let newTrs' =
               parMap
                 rdeepseq
                 (updateTrackParticlesS
                    rows
                    cols
                    frameIdx
                    stParm
                    tmParm
                    probThr
                    ps)
                 trs
         -- print newTrs'
         let trimmedNewTrs' = trimTracks frameIdx newTrs'
         -- let (trimmedNewTrs', !maxLen) = trimTracks' frameIdx newTrs'
         -- printCurrentTime
         --   (printf
         --      "%d tracks removed. Maximum length is %d."
         --      (L.length newTrs' - L.length trimmedNewTrs') maxLen)
         let unusedParticles = findUnusedParticles frameIdx trimmedNewTrs' ps
             newTrs =
               (createTracksFromParticles
                  (trackID . L.head $ newTrs')
                  frameIdx
                  stParm
                  unusedParticles) L.++
               trimmedNewTrs'
         printCurrentTime (printf "%d tracks.\n" (L.length newTrs))
         return (newTrs, frameIdx + 1))
      (tracks, 1)
      filePaths
  return results
  


{-# INLINE findTracksFromFileCheckpoint #-}
findTracksFromFileCheckpoint ::
     PrintParam
  -> StParam
  -> TranMatParam
  -> TrackingParam
  -> [Track]
  -> [FilePath]
  -> IO [Track]
findTracksFromFileCheckpoint prParam stParm tmParm trParam tracks filePaths = do
  let !pixelThr = trackingParamPixelThreshold trParam
      !probThr = trackingParamProbThreshold trParam
      !folderPath = printParamFilePath prParam </> "Checkpoints"
      !rows = printParamRows prParam
      !cols = printParamCols prParam
  createDirectoryIfMissing True folderPath
  (results, _) <-
    M.foldM
      (\(trs, frameIdx) filePath -> do
         ps <-
           filterParticles (trackingParamMaximumNumParticle trParam) <$>
           findParticlesFromFile frameIdx pixelThr filePath
         printCurrentTime (printf "%d particles found." (L.length ps))
         let newTrs' =
               parMap
                 rdeepseq
                 (updateTrackParticlesS
                    rows
                    cols
                    frameIdx
                    stParm
                    tmParm
                    probThr
                    ps)
                 trs
             trimmedNewTrs' = trimTracks frameIdx newTrs'
             unusedParticles = findUnusedParticles frameIdx trimmedNewTrs' ps
             newTrs =
               (createTracksFromParticles
                  (trackID . L.head $ newTrs')
                  frameIdx
                  stParm
                  unusedParticles) L.++
               trimmedNewTrs'
         printCurrentTime (printf "%d tracks.\n" (L.length newTrs))
         when
           (mod frameIdx 10 == 0)
           (do let outputFolder = folderPath </> (printf "%03d" frameIdx)
               printCurrentTime (printf "Write checkpoint %03d" frameIdx)
               createDirectoryIfMissing True outputFolder
               let sortedTracks =
                     L.sortOn (snd . particleCenter . L.last . trackPath) .
                     L.filter
                       (\tr ->
                          ((L.length . trackPath $ tr) >= 10) &&
                          (fst . particleCenter . L.last . trackPath $ tr) < 700) $
                     trimmedNewTrs'
               saveTracks rows cols (outputFolder </> "sum.png") sortedTracks
               when
                 (mod frameIdx 20 == 0)
                 (do MP.mapM_
                       (\(idx, tr) ->
                          saveTrackMeanStd
                            (outputFolder </> (printf "%03d.txt" idx))
                            tr) .
                       L.zip [0 :: Int ..] $
                       sortedTracks
                     MP.mapM_
                       (\(idx, tr) ->
                          saveTrack
                            rows
                            cols
                            (outputFolder </> (printf "%03d.png" idx))
                            tr) .
                       L.zip [0 :: Int ..] $
                       sortedTracks))
         return (newTrs, frameIdx + 1))
      (tracks, 1)
      filePaths
  return results

{-# INLINE findUnusedParticles #-}
findUnusedParticles :: Int -> [Track] -> [Particle] -> [Particle]
findUnusedParticles frameIdx tracks particles =
  let idxs = L.map L.head . L.group . L.sort . findUsed $ tracks
   in filterParticles idxs . L.sortOn (snd . particleID) $ particles
  where
    findUsed :: [Track] -> [Int]
    findUsed [] = []
    findUsed (t:ts) =
      let (!fIdx, !idx) = particleID . L.head . trackPath $ t
       in if fIdx == frameIdx
            then idx : findUsed ts
            else findUsed ts
    filterParticles :: [Int] -> [Particle] -> [Particle]
    filterParticles _ [] = []
    filterParticles [] ps = ps
    filterParticles idxs@(!i:is) (!p:ps) =
      if i == (snd . particleID $ p)
        then filterParticles is ps
        else p : filterParticles idxs ps


