{-# LANGUAGE BangPatterns #-}
module States where

import Analytic
import Data.Array.Repa as R
import Particles

data StParam = StParam
  { stNumDir :: Int
  , stNumSpeed :: Int
  , stMeanSpeed :: Double
  , stMinSpeed :: Double
  , stMaxSpeed :: Double
  , stSpeedSTD :: Double
  }

{-# INLINE gaussian1D #-}
gaussian1D :: (Fractional a, Floating a) => a -> a -> a
gaussian1D x std = exp (x ^ 2 / ((-2) * std ^ 2)) / std * sqrt (2 * pi)

initStateGaussian :: StParam -> State
initStateGaussian (StParam numDir numSpeed meanSpeed minSpeed maxSpeed std) =
  let deltaS = (log (maxSpeed / minSpeed)) / fromIntegral numSpeed
      logMeanSpeed = log meanSpeed
      logMinSpeed = log minSpeed
      logMaxSpeed = log maxSpeed
      arr =
        computeUnboxedS . fromFunction (Z :. numDir :. numSpeed) $ \(Z :. i :. j) ->
          let s = fromIntegral j * deltaS + logMinSpeed
           in gaussian1D (s - logMeanSpeed) std
      z = R.sumAllS arr
   in computeS . R.map (/ z) $ arr 
   
initState :: StParam -> State
initState (StParam numDir numSpeed meanSpeed minSpeed maxSpeed std) =
  let arr =
        computeUnboxedS . fromFunction (Z :. numDir :. numSpeed) $ \(Z :. i :. j) ->
          1
      z = R.sumAllS arr
   in computeS . R.map (/ z) $ arr 
   

transitionS :: StParam -> TranMatParam -> Particle -> Particle -> Particle
transitionS stParm tmParm p0 p1 =
  let minSpeed = stMinSpeed stParm
      maxSpeed = stMaxSpeed stParm
      state0 = particleState p0
      (x0, y0) = particleCenter p0
      (x1, y1) = particleCenter p1
      (Z :. numDir :. numSpeed) = extent state0
      deltaS = (log (maxSpeed / minSpeed)) / fromIntegral numSpeed
      deltaD = 2 * pi / fromIntegral numDir
      logMinSpeed = log minSpeed
      state1 =
        computeUnboxedS . fromFunction (extent state0) $ \(Z :. i :. j) ->
          let dir1 = fromIntegral i * deltaD
              speed1 = exp $ logMinSpeed + fromIntegral j * deltaS
              transitionMatrix =
                R.traverse state0 id $ \f idx@(Z :. i' :. j') ->
                  let dir0 = fromIntegral i' * deltaD
                   in if j /= j'
                        then 0
                        else f idx *
                             computePji
                               tmParm
                               (R2S1RP x0 y0 dir0 speed1)
                               (R2S1RP x1 y1 dir1 speed1)
           in sumAllS transitionMatrix
   in setState state1 p1

transitionP :: StParam -> TranMatParam -> Particle -> Particle -> IO Particle
transitionP stParm tmParm p0 p1 = do
  let minSpeed = stMinSpeed stParm
      maxSpeed = stMaxSpeed stParm
      state0 = particleState p0
      (x0, y0) = particleCenter p0
      (x1, y1) = particleCenter p1
      (Z :. numDir :. numSpeed) = extent state0
      !deltaS = (log (maxSpeed / minSpeed)) / fromIntegral numSpeed
      !deltaD = 2 * pi / fromIntegral numDir
      logMinSpeed = log minSpeed
  state1 <-
    computeUnboxedP . fromFunction (extent state0) $ \(Z :. i :. j) ->
      let !dir1 = fromIntegral i * deltaD
          !speed1 = exp $ logMinSpeed + fromIntegral j * deltaS
          transitionMatrix =
            R.traverse state0 id $ \f idx@(Z :. i' :. j') ->
              let !dir0 = fromIntegral i' * deltaD
               in if j /= j'
                    then 0
                    else f idx *
                         computePji
                           tmParm
                           (R2S1RP x0 y0 dir0 speed1)
                           (R2S1RP x1 y1 dir1 speed1)
       in sumAllS transitionMatrix
  return $ setState state1 p1

