module FindParticles where

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
  print opts
  img <- IO.read . argFilePath $ opts
  let threshold = argPixelThreshold opts
      (Z :. rows :. cols) = extent img
      graph = createGraph img threshold
      ps = findParticles graph (-1) rows cols
      folderPath = "output/test/FindParticles"
  flag <- doesPathExist folderPath
  when flag (removePathForcibly folderPath)
  createDirectoryIfMissing True folderPath
  printf "w: %d, h: %d, size: %d\n" rows cols (rows * cols)
  printf "%d particles found.\n" (L.length ps)
  -- M.mapM_ (\(Particles ps c) -> printf "size: %d center: %s\n" (L.length ps) (show c)) ps
  printf
    "Max brightness: %.2f\nMin brightness: %.2f\nAvg brightness: %.2f\n"
    (L.maximum . L.map getBrightness $ ps)
    (L.minimum . L.map getBrightness $ ps)
    ((L.sum . L.map getBrightness $ ps) / fromIntegral (L.length ps))
  -- MP.mapM_
  --   (\p ->
  --      saveParticle
  --        rows
  --        cols
  --        (folderPath </> (printf "%d.png" (particleID p)))
  --        p)
  --   ps
  -- MP.mapM_
  --   (\(p, idx) ->
  --      saveParticle rows cols (folderPath </> (printf "%d.png" idx)) p) $
  --   L.zip (L.reverse . L.sortOn getBrightness $ ps) [0 :: Int ..]
  saveParticles rows cols (folderPath </> "sum.png") ps
  printf "%d particles found." (L.length ps)
