module Workflow where

import Analytic
import Args
import IO
import Particles
import States
import Tracks

import System.FilePath

-- import Codec.FFmpeg
import Control.DeepSeq
import Control.Monad as M
import Data.Array.Repa as R
import Data.List as L
import Data.Vector.Unboxed as VU
import Data.Time
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
createInitTracks :: StParm -> [Particle] -> [Track]
createInitTracks stParm = initTracks . L.map (setState st) . L.reverse
  where
    st = initState stParm

{-# INLINE findTracksFromFile2 #-}
findTracksFromFile2 ::
     StParm -> TranMatParm -> Int -> Double -> [Track] -> [FilePath] -> IO [Track]
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
     StParm -> TranMatParm -> Int -> [Track] -> [FilePath] -> IO [Track]
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
  -> StParm
  -> TranMatParm
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
                iteration
                rows
                cols
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

{-# INLINE saveParticle #-}
saveParticle :: Int -> Int -> FilePath -> Particle -> IO ()
saveParticle rows cols filePath p =
  let xs = particlePoints p
      vec = VU.replicate (rows * cols) (0 :: Double)
      vec' =
        vec //
        (L.map
           (\(Point x y v) ->
              let i = convertIndex' cols x y
               in (i, v))
           xs)
      img' = R.fromUnboxed (Z :. rows :. cols) vec'
   in write filePath img'
   

{-# INLINE saveParticleWithCirc #-}
saveParticleWithCirc :: Int -> Int -> FilePath -> Particle -> IO ()
saveParticleWithCirc rows cols filePath p =
  let xs = particlePoints p
      vec = VU.replicate (rows * cols) (0 :: Double)
      vec' =
        vec //
        (L.map
           (\(Point x y v) ->
              let i = convertIndex' cols x y
               in (i, v))
           xs)
      img' = R.fromUnboxed (Z :. rows :. cols) vec'
   in write filePath img'
   
{-# INLINE saveTrack #-}
saveTrack :: Int -> Int -> FilePath -> Track -> IO ()
saveTrack rows cols filePath tr =
  let xs = L.concatMap particlePoints . trackPath $ tr
      vec = VU.replicate (rows * cols) (0 :: Double)
      vec' =
        VU.accum (+) vec 
        (L.map
           (\(Point x y v) ->
              let i = convertIndex' cols x y
               in (i, v))
           xs)
      img' = R.fromUnboxed (Z :. rows :. cols) vec'
   in write filePath img'
   

-- {-# INLINE saveTrackVideo #-}
-- saveTrackVideo :: Int -> Int -> FilePath -> [Track] -> IO ()
-- saveTrackVideo rows cols filePath tracks = do
--   let param = EncodingParams w h 1 Nothing Nothing " " Nothing" "
--   undefined
--   -- let xs = L.concatMap particlePoints . trackPath $ tr
--   --     vec = VU.replicate (rows * cols) (0 :: Double)
--   --     vec' =
--   --       vec //
--   --       (L.map
--   --          (\(Point x y v) ->
--   --             let i = convertIndex' cols x y
--   --              in (i, v))
--   --          xs)
--   --     img' = R.fromUnboxed (Z :. rows :. cols) vec'
--   --  in write filePath img'


{-# INLINE printCurrentTime #-}
printCurrentTime :: String -> IO ()
printCurrentTime s = do
  time <- getZonedTime
  printf
    "%s %s\n"
    (L.take 8 . show . localTimeOfDay . zonedTimeToLocalTime $ time)
    s
