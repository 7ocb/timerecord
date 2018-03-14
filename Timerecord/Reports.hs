module Timerecord.Reports
    (ReportFormat(..),
     showReportWith,
     Task(..))
where

import Timerecord.State
import Commons.Time
import Control.Applicative
import Data.Function
import Timerecord.Util (mapBy2)
import Data.Maybe
import Data.List

data ReportFormat = ReportTaskDD
                  | ReportTaskMM
                  | ReportTaskYYYY
                  | ReportTaskDescription
                  | ReportTaskTime
                  | ReportTaskProjectCode
                  | ReportTaskString String
                    deriving (Show)

data Task = Task {
      taskStartTime :: Time,
      taskMinutes :: Int,
      taskMessage :: Maybe String
    }

data JoinResult a = Pair a a
                  | Joined a

toList (Pair a b) = [a, b]
toList (Joined a) = [a]

pairwiseJoin :: (a -> a -> JoinResult a) -> [a] -> [a]
pairwiseJoin joiner [] = []
pairwiseJoin joiner (one:[]) = [one]
pairwiseJoin joiner (prev:next:rest) =
    continue [] rest (joiner prev next)
    where continue acc [] joinResult = acc ++ (toList joinResult)
          continue acc (x:xs) (Pair f n) = continue (acc ++ [f]) xs (joiner n x)
          continue acc (x:xs) (Joined j) = continue acc xs (joiner j x)

joinTask p n
    | noMessage p && noMessage n = joined p n Nothing
    | noMessage p                = joined p n $ taskMessage n
    | noMessage n = joinTask n p
    | otherwise   = joined p n $ taskMessage p

    where joined p n message = Task ((min `on` taskStartTime) p n) (((+) `on` taskMinutes) p n) message

noMessage = isNothing . taskMessage
sameMessage = (==) `on` taskMessage

toHour min = (fromIntegral min) / 60

showMessage = fromMaybe "!unknown!" . taskMessage
showHours   = show . toHour . taskMinutes

roundToHalfHour :: [Task] -> [Task]
roundToHalfHour = snd . foldr roundNext (0, [])
    where roundNext next (timeLeft, acc) = (timeLeftAfterRounding,
                                            next { taskMinutes = roundedTime} : acc)
              where roundedTime = max 30 rawRoundedTime
                    rawRoundedTime = (timeToRound `div` 30) * 30
                    timeLeftAfterRounding = timeToRound - roundedTime
                    timeToRound = timeLeft + (taskMinutes next)




tasks :: State -> [Task]
tasks = joinAndSort . catMaybes . mapBy2 toTask

    where toTask event next@(DidTask message _) = Just $ task event next (Just message)
          toTask event leave@(Leave _)          = Just $ task event leave Nothing
          toTask _ _ = Nothing

          task first next message = Task (evTime first) ((minutesAfter `on` evTime) next first) message

          joinAndSort = roundToHalfHour
                        . concat
                        . map (sortBy (compare `on` taskStartTime)
                               . joinIf sameMessage
                               . sortBy (compare `on` taskMessage)
                               . joinLastUnknown
                               . joinIf (flip $ const noMessage))
                        . groupBy (isSameDay `on` taskStartTime)


          joinLastUnknown xs@(_:_:_) = withoutTwoLasts ++ (joinIf (const noMessage) twoLasts)
              where withoutTwoLasts = (inits xs) !! splitpoint
                    twoLasts = (tails xs) !! splitpoint
                    splitpoint = length xs - 2
          joinLastUnknown lessThanTwo = lessThanTwo

          joinIf doJoin = pairwiseJoin join
              where p `join` n | doJoin p n = Joined $ p `joinTask` n
                               | otherwise  = Pair p n



          a `minsAfter` b = (evTime a) `minutesAfter` (evTime b)


showReportWith :: [ReportFormat]         -- ^ format description
               -> (Time -> Time -> Bool) -- ^ time filter
               -> (Task -> String)       -- ^ project code generator
               -> Time                   -- ^ current time
               -> State                  -- ^ full state
               -> String
showReportWith format ifAllowedByTime projectCode timeNow fullState =
    (concat $ map performFormat $ allTaks) ++ totally

    where allTaks = tasks $ leavedState
          totally = "reported totally: " ++ (show $ toHour $ sum $ map taskMinutes allTaks) ++ " hours"
          selectedEvents = filter (ifAllowedByTime timeNow . evTime) fullState

          leavedState = if isLeaved selectedEvents
                        then selectedEvents
                        else selectedEvents ++ [(Leave timeNow)]

          performFormat task = concat $ map (render task) format

          showTime f = formatTime f . taskStartTime 

          render task ReportTaskDD = showTime "%d" task
          render task ReportTaskMM = showTime "%m" task
          render task ReportTaskYYYY = showTime "%Y" task
          render task ReportTaskDescription = showMessage task
          render task ReportTaskTime = showHours task
          render task ReportTaskProjectCode = projectCode task
          render _ (ReportTaskString str) = str
