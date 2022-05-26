module Args  where

import System.Console.GetOpt
import System.Environment

data Flag
  = FilePath String
  | FolderPath String
  | PixelThreshold Int
  
data Options = Options
  { argFilePath :: String
  , argFolderPath :: String
  , argOutputFolderPath :: String
  , argPixelThreshold :: Int
  , argProbThreshold :: Double
  , argSigma :: Double
  , argTau :: Double
  , argNumDir :: Int
  , argNumSpeed :: Int
  , argMeanSpeed :: Double
  , argMinSpeed :: Double
  , argMaxSpeed :: Double
  , argSpeedSTD :: Double
  , argMaxNumParticle :: Int
  } deriving (Show)
  
defaultOptions :: Options
defaultOptions =
  Options
    { argFilePath = ""
    , argFolderPath = ""
    , argOutputFolderPath = ""
    , argPixelThreshold = -1
    , argProbThreshold = 0
    , argSigma = 0.018
    , argTau = 9
    , argNumDir = 72
    , argNumSpeed = 72
    , argMeanSpeed = 1
    , argMinSpeed = 0.5
    , argMaxSpeed = 2
    , argSpeedSTD = 0.5
    , argMaxNumParticle = 50
    }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option
      "f"
      ["filePath"]
      (ReqArg (\arg opt -> return opt {argFilePath = arg}) "FILE")
      "Input file"
  , Option
      "F"
      ["folderPath"]
      (ReqArg (\arg opt -> return opt {argFolderPath = arg}) "DIR")
      "Input folder"
  , Option
      "O"
      ["outputFolderPath"]
      (ReqArg (\arg opt -> return opt {argOutputFolderPath = arg}) "DIR")
      "Output folder"
  , Option
      "p"
      ["pixelThreshold"]
      (ReqArg
         (\arg opt -> return opt {argPixelThreshold = read arg :: Int})
         "VALUE")
      "Threshold for pixels"
  ,  Option
       "P"
       ["probThreshold"]
       (ReqArg
          (\arg opt -> return opt {argProbThreshold = read arg :: Double})
          "VALUE")
       "Threshold for transition probablity"
  , Option
      "s"
      ["sigma"]
      (ReqArg (\arg opt -> return opt {argSigma = read arg :: Double}) "VALUE")
      "The sigma parameter for the analytic Green's function"
  , Option
      "t"
      ["tau"]
      (ReqArg (\arg opt -> return opt {argTau = read arg :: Double}) "VALUE")
      "The tau parameter for the analytic Green's function"
  , Option
      "d"
      ["numDir"]
      (ReqArg (\arg opt -> return opt {argNumDir = read arg :: Int}) "VALUE")
      "Number of directions"
  , Option
      "z"
      ["numSpeed"]
      (ReqArg (\arg opt -> return opt {argNumSpeed = read arg :: Int}) "VALUE")
      "Number of Speed."
  , Option
      "z"
      ["meanSpeed"]
      (ReqArg
         (\arg opt -> return opt {argMeanSpeed = read arg :: Double})
         "VALUE")
      "The mean of initial speed distribution."
  , Option
      "z"
      ["minSpeed"]
      (ReqArg
         (\arg opt -> return opt {argMinSpeed = read arg :: Double})
         "VALUE")
      "The minimum of initial speed distribution."
  , Option
      "z"
      ["maxSpeed"]
      (ReqArg
         (\arg opt -> return opt {argMaxSpeed = read arg :: Double})
         "VALUE")
      "The maximum of initial speed distribution."
  , Option
      "z"
      ["speedSTD"]
      (ReqArg
         (\arg opt -> return opt {argSpeedSTD = read arg :: Double})
         "VALUE")
      "The STD of initial speed distribution."
  ,  Option
       "z"
       ["maxNumParticle"]
       (ReqArg
          (\arg opt -> return opt {argMaxNumParticle = read arg :: Int})
          "VALUE")
       "The maximum number of particles found in one frame."
  ]
