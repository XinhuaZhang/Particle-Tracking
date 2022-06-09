{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Tracks where

import Analytic
import IO
import Particles
import Plot
import States

import Control.Monad as M
import Control.Parallel.Strategies
import Data.Array.Repa as R
import Data.List as L
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Text.Printf

import System.Directory
import System.FilePath
import System.IO

data Track = Track
  { trackID :: Int
  , trackBirthday :: Int
  , trackPath :: [Particle]
  } deriving (Generic, Show)

instance NFData Track

{-# INLINE getTrackLength #-}
getTrackLength :: Track -> Int
getTrackLength = L.length . trackPath

{-# INLINE setTrackBirthday #-}
setTrackBirthday :: Int -> Track -> Track
setTrackBirthday bd (Track tID _ path) = Track tID bd path

{-# INLINE addPartcileTrack #-}
addPartcileTrack :: Particle -> Track -> Track
addPartcileTrack p (Track tID tBd path) = Track tID tBd (p : path)

{-# INLINE initTracks #-}
initTracks :: [Particle] -> [Track]
initTracks = L.reverse . L.zipWith (\idx p -> Track idx 0 [p]) [0 ..]

{-# INLINE createTracksFromParticles #-}
createTracksFromParticles :: Int -> Int -> StParam -> [Particle] -> [Track]
createTracksFromParticles prevTrackID bd stParam =
  L.reverse . L.zipWith (\idx p -> Track idx bd [setState st p]) [prevTrackID + 1 ..]
  where st = initState stParam

normalizeTracksState :: [Track] -> [Track]
normalizeTracksState =
  parMap
    rdeepseq
    (\(Track idx tBd (p:ps)) -> Track idx tBd ((normalizeParticleState p) : ps))

updateTracks1 :: StParam -> TranMatParam -> Int -> [Track] -> Particle -> [Track]
updateTracks1 stParm tmParm thr tracks p1 =
  let xs =
        parMap
          rdeepseq
          (\tr ->
             let tID = trackID tr
                 ps = trackPath tr
                 p0 = L.head ps
                 newP1 = transitionS stParm tmParm p0 p1
                 v = sumAllS . particleState $ newP1
              in (tID, newP1, v))
          tracks
      (maxTID, maxV, p) =
        L.foldl'
          (\(tIDMax, maxV, pMax) (tID, p, v) ->
             if v > maxV
               then (tID, v, p)
               else (tIDMax, maxV, pMax))
          (-1, -1, undefined)
          xs
   in update maxTID p tracks

updateTracks2 ::
     Int -> StParam -> TranMatParam -> Double -> Int -> [Track] -> Particle -> IO [Track]
updateTracks2 iteration stParm tmParm thr maxLen tracks p1 = do
  let (x1, y1) = particleCenter p1
  printf "pID = %s\n" (show $ particleID p1)
  xs <-
    M.mapM
      (\tr ->
         let ps = trackPath tr
          in if L.length ps > maxLen
               then return (-1, undefined, -1, undefined, -1)
               else do
                 let p0 = L.head ps
                     (x0, y0) = particleCenter p0
                     d = sqrt $ (x0 - x1) ^ 2 + (y0 - y1) ^ 2
                     tID = trackID tr
                 newP1 <- transitionP stParm tmParm p0 p1
                 v <- sumAllP . particleState $ newP1
                 return (tID, newP1, v, p0, d))
      tracks
  let ys = L.take 2 . L.reverse . L.sortOn (\(_, _, v, _, _) -> v) $ xs
  M.mapM_
    (\(tID, _, v, p0, d) ->
       when
         (v > 0)
         (let (x0, y0) = particleCenter p0
           in printf
                "track ID: %d P0: (%.2f, %.2f) P1: (%.2f, %.2f) %.2f (%.2f, %.2f) V: %.4f\n"
                tID
                x0
                y0
                x1
                y1
                d
                (x1 - x0)
                (y1 - y0)
                v))
    ys
  let defaultP1 = setState (initState stParm) p1
      (maxTID, maxV, p, p0) =
        L.foldl'
          (\(tIDMax, maxV, pMax, p0Max') (tID, p, v, p0', _) ->
             if v > maxV
               then (tID, v, p, p0')
               else (tIDMax, maxV, pMax, p0Max'))
          (-1, -1, defaultP1, undefined)
          xs
  -- let (x0, y0) = particleCenter p0
  -- printf "track ID: %d P0: (%.2f, %.2f) P1: (%.2f, %.2f)\n" maxTID x0 y0 x1 y1
  -- return $ update maxTID p tracks
  -- printf "maxV: %.2f thr: %.1f\n" maxV thr
  if maxV > thr
    then return $ update maxTID (normalizeParticleState p) tracks
    else let tID = 1 + (trackID . L.head $ tracks)
          in return $ (Track tID iteration [defaultP1]) : tracks


updateTracks3 ::
     FilePath
  -> Int
  -> Int
  -> Int
  -> StParam
  -> TranMatParam
  -> Double
  -> Int
  -> [Track]
  -> Particle
  -> IO [Track]
updateTracks3 folderPath rows cols iteration stParm tmParm thr maxLen tracks p1 = do
  let (x1, y1) = particleCenter p1
  -- printf "pID = %d\n" (particleID p1)
  -- let outputFolder = folderPath </> (printf "%d_%d" iteration (particleID p1))
  -- createDirectoryIfMissing True outputFolder
  -- saveParticle rows cols (outputFolder </> printf "1_%.1f_%.1f.png" x1 y1) p1
  xs <-
    M.mapM
      (\tr ->
         let ps = trackPath tr
         in if L.length ps > maxLen
              then return (-1, undefined, -1, L.last ps, -1)
              else do
                let tID = trackID tr
                    p0 = L.head ps
                    (x0, y0) = particleCenter p0
                    d = sqrt $ (x0 - x1) ^ 2 + (y0 - y1) ^ 2
                newP1 <- transitionP stParm tmParm p0 p1
                v <- sumAllP . particleState $ newP1
                return (tID, newP1, v, p0, d))
      tracks
  -- let ys = L.take 1 . L.reverse . L.sortOn (\(_, _, v, _, _) -> v) $ xs
  -- M.mapM_
  --   (\(tID, _, v, p0, d)
  --      -- when
  --      --   (v > 0)
  --     ->
  --      (do let (x0, y0) = particleCenter p0
  --          saveParticle
  --            rows
  --            cols
  --            (outputFolder </> printf "0_%.1f_%.1f.png" x0 y0)
  --            p0
  --          printf
  --            "track ID: %d P0: (%.2f, %.2f) P1: (%.2f, %.2f) %.2f (%.2f, %.2f) V: %.4f\n"
  --            tID
  --            x0
  --            y0
  --            x1
  --            y1
  --            d
  --            (x1 - x0)
  --            (y1 - y0)
  --            v))
  --   ys
  let defaultP1 = setState (initState stParm) p1
      (maxTID, maxV, p, p0) =
        L.foldl'
          (\(tIDMax, maxV, pMax, p0Max') (tID, p, v, p0', _) ->
             if v > maxV
               then (tID, v, p, p0')
               else (tIDMax, maxV, pMax, p0Max'))
          (-1, -1, defaultP1, undefined)
          xs
  if maxV > thr
    then return $ update maxTID (normalizeParticleState p) tracks
    else let tID = 1 + (trackID . L.head $ tracks)
          in return $ (Track tID iteration [defaultP1]) : tracks


updateTrackParticlesDebug ::
     FilePath
  -> Int
  -> Int
  -> Int
  -> StParam
  -> TranMatParam
  -> Double
  -> [Particle]
  -> Track
  -> IO Track
updateTrackParticlesDebug folderPath rows cols iteration stParm tmParm thr ps track = do
  let p0 = L.head . trackPath $ track
      (x0, y0) = particleCenter p0
  -- printf "Track ID: %d\n" (trackID track)
  -- let outputFolder = folderPath </> (printf "%d" iteration)
  -- createDirectoryIfMissing True outputFolder
  -- -- saveParticle rows cols (outputFolder </> printf "1_%.1f_%.1f.png" x1 y1) p1
  -- plotDirection (outputFolder </> printf "%d.png" (trackID track)) p0
  xs <-
    M.mapM
      (\p1 -> do
         let (x1, y1) = particleCenter p1
             pID = particleID p1
             d = sqrt $ (x0 - x1) ^ 2 + (y0 - y1) ^ 2
         newP1 <- transitionP stParm tmParm p0 p1
         v <- sumAllP . particleState $ newP1
         return (pID, newP1, v, d))
      ps
  -- let ys = L.take 2 . L.reverse . L.sortOn (\(_, _, v, _) -> v) $ xs
  -- M.mapM_
  --   (\(pID, p1, v, d) -> do
  --      let (x1, y1) = particleCenter p1
  --          -- saveParticle
  --          --   rows
  --          --   cols
  --          --   (outputFolder </> printf "0_%.1f_%.1f.png" x0 y0)
  --          --   p0
  --      printf
  --        "Particle ID: %d P0: (%.2f, %.2f) P1: (%.2f, %.2f) %.2f (%.2f, %.2f) V: %.4f\n"
  --        pID
  --        x0
  --        y0
  --        x1
  --        y1
  --        d
  --        (x1 - x0)
  --        (y1 - y0)
  --        v)
  --   ys
  let (maxPID', maxV', maxP') =
        L.foldl'
          (\(maxPID, maxV, maxP) (pID, p1, v, d) ->
             if v > maxV
               then (pID, v, p1)
               else (maxPID, maxV, maxP))
          ((-1, -1), -1, undefined)
          xs
  if maxV' > thr
    then return $ addPartcileTrack (normalizeParticleState maxP') track
    else return track
    

updateTrackParticlesS ::
     Int
  -> Int
  -> Int
  -> StParam
  -> TranMatParam
  -> Double
  -> [Particle]
  -> Track
  -> Track
updateTrackParticlesS rows cols iteration stParm tmParm thr ps track =
  let p0 = L.head . trackPath $ track
      (!x0, !y0) = particleCenter p0
      xs =
        L.map
          (\p1
            ->
             let !pID = particleID p1              
                 newP1 = transitionS stParm tmParm p0 p1
                 !v = sumAllS . particleState $ newP1
              in (pID, newP1, v))
          ps
      (maxPID', maxV', maxP') =
        L.foldl'
          (\(maxPID, maxV, maxP) (pID, p1, v) ->
             if v > maxV
               then (pID, v, p1)
               else (maxPID, maxV, maxP))
          ((-1, -1), -1, undefined)
          xs
   in if maxV' > thr
        then addPartcileTrack (normalizeParticleState maxP') track
        else track

update :: Int -> Particle -> [Track] -> [Track]
update tIDMax _ [] = error (printf "update: Empty tracks. tIDMax = %d" tIDMax)
update tIDMax p (t:ts) =
  if tIDMax == trackID t
    then (addPartcileTrack p t) : ts
    else t : update tIDMax p ts
    

trimTracks :: Int -> [Track] -> [Track]
trimTracks _ [] = []
trimTracks n (t:ts) =
  let !bd =  trackBirthday t
      (!a, !b) = divMod (n - bd) 5
  in if getTrackLength t <= 10 && b == 0 && getTrackLength t <= 1 * a && n > bd
       then trimTracks n ts
       else t : trimTracks n ts
       

trimTracks' :: Int -> [Track] -> ([Track], Int)
trimTracks' _ [] = ([], -1)
trimTracks' n (t:ts) =
  let !bd = trackBirthday t
      (!a, !b) = divMod (n - bd) 5
      (xs, !maxLen) = trimTracks' n ts
   in if b == 0 && getTrackLength t <= 3 * a && n > bd
        then (xs, max (getTrackLength t) maxLen)
        else (t : xs, maxLen)

{-# INLINE removeDuplicateTracks #-}
removeDuplicateTracks :: [Track] -> [Track]
removeDuplicateTracks =
  L.reverse .
  L.sortOn trackID .
  removeDuplicateAdjacentTracks . L.sortOn (L.head . trackPath)


removeDuplicateAdjacentTracks :: [Track] -> [Track]
removeDuplicateAdjacentTracks (x: []) = [x]
removeDuplicateAdjacentTracks (x:y:xs) =
  let as = trackPath x
      bs = trackPath y
      n = compareTracks 0 as bs
      asLen = L.length as
      bsLen = L.length bs
      m = min asLen bsLen
   in if fromIntegral n / fromIntegral m > 0.8
        then if asLen < bsLen
               then removeDuplicateAdjacentTracks (y : xs)
               else removeDuplicateAdjacentTracks (x : xs)
        else x : removeDuplicateAdjacentTracks (y : xs)
  where
    compareTracks :: Int -> [Particle] -> [Particle] -> Int
    compareTracks !n [] _ = n
    compareTracks !n _ [] = n
    compareTracks !n (a:as) (b:bs)
      | a == b = compareTracks (n + 1) as bs
      | otherwise = n

{-# INLINE saveTrack #-}
saveTrack :: Int -> Int -> FilePath -> Track -> IO ()
saveTrack rows cols filePath tr =
  let xs =
        L.map
          (\ps ->
             let n = L.length ps
                 s = L.sum . L.map (\(Point _ _ v) -> v) $ ps
                 (Point x y _) = L.head ps
              in Point x y (s / fromIntegral n)) .
        L.group . L.sort . L.concatMap particlePoints . trackPath $
        tr
      vec = VU.replicate (rows * cols) (0 :: Double)
      vec' =
        VU.accum
          (+)
          vec
          (L.map
             (\(Point x y v) ->
                let i = convertIndex' cols x y
                 in (i, v))
             xs)
      img' = R.fromUnboxed (Z :. rows :. cols) vec'
   in write filePath img'

{-# INLINE saveTrackMeanStd #-}
saveTrackMeanStd :: FilePath -> Track -> IO ()
saveTrackMeanStd filePath tr =
  withFile filePath WriteMode $ \h ->
    M.mapM_
      (\p ->
         let (!cX, !cY) = particleCenter p
             (!stdX, !stdY) = getParticleStd p
          in hPutStrLn h (printf "%.2f %.2f %.2f %.2f" cX cY stdX stdY)) .
    L.reverse . trackPath $
    tr


{-# INLINE saveTracks #-}
saveTracks :: Int -> Int -> FilePath -> [Track] -> IO ()
saveTracks rows cols filePath tracks = do
  let xs =
        L.map
          (\ps ->
             let n = L.length ps
                 s = L.sum . L.map (\(Point _ _ v) -> v) $ ps
                 (Point x y _) = L.head ps
              in Point x y (s / fromIntegral n)) .
        L.group . L.sort . L.concatMap particlePoints . L.concatMap trackPath $
        tracks
      vec = VU.replicate (rows * cols) (0 :: Double)
      vec' =
        VU.accum
          (+)
          vec
          (L.map
             (\(Point x y v) ->
                let i = convertIndex' cols x y
                 in (i, v))
             xs)
      img' = R.fromUnboxed (Z :. rows :. cols) vec'
   in write filePath img'
