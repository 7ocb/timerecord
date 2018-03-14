module Timerecord.State
    (Event(..),
    isEnter,
    isLeave,
    isLeaved,
    leavesAndEnters,
    versionStorage,
    register,
    moveToTime,
    evTime,
    evName,
    transformCurrentState,
    State,
    getCurrentMonthState)
where

import Commons.Time

import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.List (find, sortBy)
import Data.Function (on)
import qualified Timerecord.IO as TrIO
import Timerecord.Util (mapBy2)
import System.FilePath ((</>))
import Control.Monad (liftM2, fmap)
import Control.Monad.Reader (asks, liftIO)
import Control.Applicative

import Timerecord.ExecutionTime

data Event = Enter Time
           | Leave Time
           | DidTask String Time -- task
             deriving (Show, Read, Eq)

versionStorage = 2

isEnter (Enter _) = True
isEnter _ = False

isLeave (Leave _) = True
isLeave _ = False

isDidTask (DidTask _ _) = True
isDidTask _ = False

evName (Enter _) = "enter"
evName (Leave _) = "leave"
evName (DidTask _ _) = "did task"

evTime (Enter time) = time
evTime (Leave time) = time
evTime (DidTask _ time) = time

moveToTime (Enter _) = Enter 
moveToTime (Leave _) = Leave
moveToTime (DidTask msg _) = DidTask msg


isLeaved :: [Event] -> Bool
isLeaved = isLeave . last . leavesAndEnters

leavesAndEnters :: [Event] -> [Event]
leavesAndEnters = filter ((||) <$> isLeave <*> isEnter) 

type StateError = Maybe String

type State = [Event]

placementError :: Event -> Event -> StateError
placementError after before =
    snd `fmap` find fst [(not $ after `canBeAfter` before,
                          (evName after) ++ " can't be place after " ++ (evName before)),
                         ((evTime after) < (evTime before),
                          "Element have time (" ++ (show $ evTime after) ++ ") before previous (" ++ (show $ evTime before) ++ ")")]

    where (Leave _) `canBeAfter` (Enter _) = True
          (Leave _) `canBeAfter` (DidTask _ _) = True
          (Enter _) `canBeAfter` (Leave _) = True
          (DidTask _ _) `canBeAfter` (Enter _) = True
          (DidTask _ _) `canBeAfter` (DidTask _ _) = True
          _ `canBeAfter` _ = False

findError :: State -> StateError
findError [] = Nothing
findError (event:[]) = if (isEnter event)
                       then Nothing
                       else Just "First element must be enter"

findError events = fromJust `fmap` (find isJust $ mapBy2 (flip placementError) events)

register :: Event -> IOAtTime ()
register event = do
  let name = (evName event)
  message $ "Registering " ++ name ++ " at " ++ (show $ evTime event)
  transformCurrentState $ (++ [event])


-- stoage

data EventInStorage = SEnter String
                    | SLeave String
                    | STask String String
                      deriving (Show, Read)

storageTimeFormat = "%Y.%m.%d %H:%M:%S"

toStorageType = convert
    where convert (Enter time) = SEnter $ timeToString time
          convert (Leave time) = SLeave $ timeToString time
          convert (DidTask message time) = STask message $ timeToString time

          timeToString = formatTime storageTimeFormat

fromStorageType timeNow = convert
    where convert (SEnter time) = Enter $ timeFromString time
          convert (SLeave time) = Leave $ timeFromString time
          convert (STask message time) = DidTask message $ timeFromString time

          timeFromString = fromJust . parseTime timeNow storageTimeFormat


storeableState :: Time -> Storeable State
storeableState time = (Storeable
                       []
                       (map (fromStorageType time) . read)
                       (show . map toStorageType)
                       (++ ".v" ++ show versionStorage ++ ".txt"))

getCurrentMonthState :: IOAtTime State
getCurrentMonthState = execTime >>= get . storeableState

transformCurrentState :: (State -> State) -> IOAtTime ()
transformCurrentState transform = do
  original <- getCurrentMonthState

  let newState = sortBy (compare `on` evTime) $ transform original

  case findError newState of 
    Nothing -> execTime >>= (`put` newState) . storeableState
    Just error -> throwE error