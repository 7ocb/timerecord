module Timerecord.Config
    (Config,
     getConfig,
     -- correctedWorkdays,
     DayCorrection(..),
     CorrectType(..),
     corrections,
     correctDays,
     transformConfig)
where

import Data.List (nub, sort)
import Control.Monad (liftM2, fmap)
import Control.Monad.Reader (liftIO)    
import System.FilePath ((</>))
import Data.Maybe (fromMaybe)
import Timerecord.ExecutionTime

import qualified Timerecord.IO as TrIO

data CorrectType = Add | Remove
                 deriving (Show, Read, Eq)

isAdd Add = True
isAdd _ = False

isRemove Remove = True
isRemove _ = False

data DayCorrection = DayCorrection {
      day::Int,
      correctType :: CorrectType
    } deriving (Show, Read, Eq)

data Config = Config {
      corrections :: [DayCorrection]
    } deriving (Show, Read, Eq)

-- correctedWorkdays :: Config -> [Int] -> [Int]
-- correctedWorkdays config days =
--     sort $ nub $ (added ++) $ filter (not . (`elem` removed)) days
--     where added = daysOfType isAdd
--           removed = daysOfType isRemove

--           daysOfType t = map day $ filter (t . correctType) $ corrections config

correctDays :: [DayCorrection] -> Config -> Config
correctDays new config = config { corrections = updated }
    where updated = new ++ (filter (not.presentInNew) $ corrections config)
          presentInNew = (`elem` daysInNew) . day
          daysInNew = map day new
                      
storeableConfig = Storeable (Config []) read show (++ ".config.v1.txt")
                      
getConfig :: IOAtTime Config
getConfig = get storeableConfig

transformConfig :: (Config -> Config) -> IOAtTime ()
transformConfig = transform storeableConfig 