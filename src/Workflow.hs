module Workflow where

import Analytic
import Args
import IO
import Particles
import States
import Tracks


import Control.DeepSeq
import Control.Monad as M
import Data.Array.Repa as R
import Data.List as L
import Data.Vector.Unboxed as VU
import System.FilePath
import Text.Printf


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
findParticlesFromFile :: Int -> FilePath  -> IO [Particle]
findParticlesFromFile thr filePath = do
  printCurrentTime filePath
  img <- IO.read filePath
  let (Z :. rows :. cols) = extent img
      graph = createGraph img thr
      ps = findParticles graph rows cols
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
         ps <- findParticlesFromFile pixelThr filePath
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
       ps <- findParticlesFromFile thr filePath
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
           findParticlesFromFile pixelThr filePath
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
      (\(trs, iteration) filePath -> do
         ps <-
           filterParticles (trackingParamMaximumNumParticle trParam) <$>
           findParticlesFromFile pixelThr filePath
         printCurrentTime . show . L.length $ ps
         newTrs <- M.mapM (updateTrackParticlesDebug folderPath rows cols iteration stParm tmParm probThr ps) trs
         printCurrentTime . show . L.length $ newTrs
         return (newTrs, iteration + 1))
      (tracks, 1)
      filePaths
  return results
