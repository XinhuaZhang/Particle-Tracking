{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Particles
  ( Point(..)
  , Particle(..)
  , State
  , setState
  , normalizeParticleState
  , getBrightness
  , createGraph
  , findParticles
  , convertIndex'
  , filterParticles
  , saveParticle
  ) where
  
import IO

import Control.DeepSeq
import Data.Vector.Unboxed as VU
import Data.Array.Repa as R
import Data.Hashable
import Data.HashMap.Strict as HM
import Data.List as L
import GHC.Generics (Generic)

data Point =
  Point !Int !Int !Double  deriving (Generic)

instance Hashable Point
instance NFData Point


instance Eq Point where
  (==) (Point x1 y1 _ ) (Point x2 y2 _ ) = x1 == x2 && y1 == y2
                                           
type State = R.Array U DIM2 Double                                
     
data Particle = Particle
  { particlePoints :: [Point]
  , particleCenter :: (Double, Double)
  , particleID :: Int
  , particleState :: State
  } deriving (Generic)

instance NFData Particle where
  rnf (Particle !_ !_ !_ st) = deepSeqArray st ()
  
instance Show Particle where
  show (Particle _ c idx _) = show idx L.++ ": " L.++ show c

{-# INLINE setState #-}
setState ::  State -> Particle -> Particle
setState st (Particle xs c pid _) = Particle xs c pid st

{-# INLINE normalizeParticleState #-}
normalizeParticleState :: Particle -> Particle
normalizeParticleState p =
  let st = particleState p
      z = sumAllS st
   in setState (computeS . R.map (/ z) $ st) p 
   
{-# INLINE getBrightness #-}
getBrightness :: Particle -> Double
getBrightness = L.maximum . L.map (\(Point _ _ v) -> v) . particlePoints


{-# INLINE emptyArray #-}
emptyArray :: State
emptyArray = fromUnboxed (Z :. 0 :. 0) VU.empty

{-# INLINE updateParticleCenter #-}
updateParticleCenter :: Particle -> Particle
updateParticleCenter (Particle pos _ pid pstate) =
  let (sumX, sumY, n) =
        L.foldl'
          (\(x', y', m) (Point x y _) -> (x' + x, y' + y, m + 1))
          (0, 0, 0)
          pos
   in Particle
        pos
        (fromIntegral sumX / fromIntegral n, fromIntegral sumY / fromIntegral n)
        pid
        pstate

createGraph :: R.Array U DIM2 Double -> Int -> HashMap Point Particle
createGraph img threshold =
  fst $
  VU.ifoldl'
    (\(graph, idx) i pVal ->
       if pVal > fromIntegral threshold
         then let (x, y) = convertIndex cols i
                  key = Point x y (img R.! (Z :. x :. y))
                  val =
                    Particle
                      (L.map
                         (\(x', y', _) -> Point x' y' (img R.! (Z :. x' :. y'))) .
                       L.filter (\(_, _, flag) -> flag) .
                       L.map (\(x', y') -> check (x + x') (y + y')) $
                       offsets)
                      (0, 0)
                      idx
                      emptyArray
               in (HM.insert key val graph, idx + 1)
         else (graph, idx))
    (HM.empty, 0)
    (R.toUnboxed img)
  where
    (Z :. rows :. cols) = extent img
    offsets =
      [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    {-# INLINE check #-}
    check :: Int -> Int -> (Int, Int, Bool)
    check x y =
      ( x
      , y
      , (x >= 0) &&
        (x < rows) &&
        (y >= 0) &&
        (y < cols) && ((img R.! (Z :. x :. y)) > fromIntegral threshold))

{-# INLINE findParticles #-}
findParticles :: HashMap Point Particle -> Int -> Int -> [Particle]
findParticles map rows cols =
  let xs = keys map
      (ys, _, _) =
        L.foldl'
          (\(ys, isVisited, idx) p ->
             let (Point x y _) = p
                 i = convertIndex' cols x y
              in if isVisited VU.! i
                   then (ys, isVisited, idx)
                   else let (isVisited', zs) = dfs map isVisited cols [p]
                            ps =
                              updateParticleCenter $
                              Particle zs (0, 0) idx emptyArray
                         in (ps : ys, isVisited', idx + 1))
          ([], VU.replicate (rows * cols) False, 0)
          xs
   in ys
   
dfs ::
     HashMap Point Particle
  -> VU.Vector Bool
  -> Int
  -> [Point]
  -> (VU.Vector Bool, [Point])
dfs _ isVisited _ [] = (isVisited, [])
dfs graph isVisited cols (p:ps) =
  let (Point x y _) = p
      i = convertIndex' cols x y
   in if isVisited VU.! i
        then dfs graph isVisited cols ps
        else let zs = particlePoints $ graph HM.! p
                 isVisited' = isVisited // [(i, True)]
                 (m, ys) = dfs graph isVisited' cols (zs L.++ ps)
              in (m, p : ys)

{-# INLINE convertIndex #-}
convertIndex :: Int -> Int -> (Int, Int)
convertIndex cols i = (i `div` cols, i `mod` cols)

{-# INLINE convertIndex' #-}
convertIndex' :: Int -> Int -> Int -> Int
convertIndex' cols x y = x * cols + y

-- Preserve bright particles
{-# INLINE filterParticles #-}
filterParticles :: Int -> [Particle] -> [Particle]
filterParticles n = L.take n . L.reverse . L.sortOn getBrightness


{-# INLINE saveParticle #-}
saveParticle :: Int -> Int -> FilePath -> Particle -> IO ()
saveParticle rows cols filePath p =
  let xs = particlePoints p
      vec = VU.replicate (rows * cols) (0 :: Double)
      vec' =
        vec VU.//
        (L.map
           (\(Point x y v) ->
              let i = convertIndex' cols x y
               in (i, 65535))
           xs)
      img' = R.fromUnboxed (Z :. rows :. cols) vec'
   in write filePath img'
