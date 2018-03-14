module Commons.Time (Time,
                     currentTime,
                     weekDay,
                     withDay,
                     withHour,
                     withMinute,
                     DayInMonth(..),
                     WeekDay(..),
                     isSameWeek,
                     isSameDay,
                     isSameMonth,
                     formatTime,
                     parseTime,
                     minutesInPast,
                     minutesAfter,
                     day,
                     hour,
                     minute,
                     midnight,
                     workdaysInMonth,
                     addHours,
                     addMinutes) where

import Control.Monad (liftM2,
                      liftM,
                      fmap)

import qualified Data.Dates as DT
import Data.Dates (WeekDay(..))
import qualified Data.Time as T
import Data.Time.Calendar (gregorianMonthLength, toGregorian)
import Data.Time.Clock (secondsToDiffTime, addUTCTime)

data Time = Time T.TimeZone T.LocalTime deriving (Show, Eq, Read, Ord)

currentTime :: IO (Time)
currentTime = do
  tz <- T.getCurrentTimeZone
  utcTime <- T.getCurrentTime

  return $ Time tz $ T.utcToLocalTime tz utcTime

isTimeEqualBy :: Eq a => (T.LocalTime -> a) -> Time -> Time -> Bool
isTimeEqualBy f (Time _ time) (Time _ otherTime) =
    (f time) == (f otherTime)

isSameWeek :: Time -> Time -> Bool
isSameWeek = isTimeEqualBy $ DT.lastMonday . DT.dayToDateTime . T.localDay

isSameDay :: Time -> Time -> Bool
isSameDay = isTimeEqualBy T.localDay

isSameMonth :: Time -> Time -> Bool
isSameMonth = isTimeEqualBy $ month . T.toGregorian . T.localDay
              where month (_, m, _) = m


parseTime :: Time -> String -> String -> Maybe Time
parseTime (Time tz _) format string =
    Time tz `fmap` T.parseTime T.defaultTimeLocale format string

addHours t hours = t `addSeconds` (hours * 60 * 60)
addMinutes t minutes = t `addSeconds`  (minutes * 60)

minutesInPast minutes t = t `addSeconds` (- (minutes * 60))

addSeconds :: Time -> Int -> Time
addSeconds (Time tz localTime) seconds  =
    Time tz shiftedTime
    where shiftedTime = fromUtc $ addUTCTime (fromIntegral seconds) $ toUtc localTime
          toUtc = T.localTimeToUTC tz
          fromUtc = T.utcToLocalTime tz

withDay :: Time -> Int -> Time
withDay (Time tz localTime@(T.LocalTime d t)) newDay = Time tz $ T.LocalTime newLocalTime t
    where newLocalTime = T.fromGregorian y m newDay
          (y, m, _) = T.toGregorian $ T.localDay localTime

withHour :: Time -> Int -> Time
withHour = modTime mod
    where mod (T.TimeOfDay _ m p) newHour = T.TimeOfDay newHour m p

withMinute :: Time -> Int -> Time
withMinute = modTime mod
    where mod (T.TimeOfDay h _ p) newMinute = T.TimeOfDay h newMinute p

modTime :: (T.TimeOfDay -> Int -> T.TimeOfDay) -> Time -> Int -> Time
modTime mod (Time tz (T.LocalTime d tod)) i = Time tz $ T.LocalTime d $ mod tod i

-- newLocalTime = T.fromGregorian y m newDay
--           (y, m, _) = T.toGregorian $ 

formatTime :: String -> Time -> String
formatTime format (Time tz time) = T.formatTime T.defaultTimeLocale format time

getTime action (Time _ (T.LocalTime _ time)) = action time

hour = getTime T.todHour
minute = getTime T.todMin

day :: Time -> Int
day (Time _ localTime) = day
    where (_,_,day) = toGregorian $ T.localDay localTime

midnight (Time tz (T.LocalTime day _)) = Time tz $ T.LocalTime day T.midnight

data DayInMonth = DayInMonth {
      dayStart :: Time,
      isWorking :: Bool
    } deriving (Show, Eq)

weekDay :: Time -> WeekDay
weekDay (Time tz time) = DT.dateWeekDay $ DT.dayToDateTime $ T.localDay time

workdaysInMonth :: Time -> [DayInMonth]
workdaysInMonth (Time tz timeNow) =
    map (toDayInMonth . toTime) $ take thisMonthLength [1..]

    where yearNow = (fromIntegral (DT.year dateTime))
          monthNow = (DT.month dateTime)

          toDayInMonth day = DayInMonth day $ isWorkDay day
          toTime dayInMonth = Time tz $ T.LocalTime (T.fromGregorian yearNow monthNow dayInMonth) T.midnight

          thisMonthLength = gregorianMonthLength yearNow monthNow
          dateTime = DT.dayToDateTime $ T.localDay timeNow

          isWorkDay time = not $ (weekDay time) `elem` [Sunday, Saturday]




minutesAfter later earlier = (hour later - hour earlier) * 60 + (minute later - minute earlier)
