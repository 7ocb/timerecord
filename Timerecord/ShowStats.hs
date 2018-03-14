module Timerecord.ShowStats (printMonthCalendar, 
                             showStats,
                             Field(..))
where 

import Timerecord.ExecutionTime
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.List (concatMap, find, intercalate, elemIndex)
import Text.Printf (printf)
import Timerecord.Config (Config)
import qualified Timerecord.Config as Cfg
import Timerecord.State hiding (leavesAndEnters)
import qualified Timerecord.State as S
import Timerecord.Util (mapBy2)
import Commons.Time

import Control.Applicative ((<$>), (*>), (<*), (<*>))

data Field = Preformat String
           | CurrentState
           | TodayIfLeaveNow
           | LeftForToday
           | WeekBeforeToday
           | MonthBeforeThisWeek
           | MonthBeforeThisWeekRequired
           | MonthBeforeToday
           | MonthBeforeTodayRequired
           | MonthTimesDiff
           | AvgDayToFullWorkload
           | WorkdayEndsAt
           | RecommendedLeaveTime
           | WorkdaysInMonth
           | WorkdaysPassed
           | WorkdaysLeftIncludingToday

correctedWorkdays :: Time -> Config -> [DayInMonth]
correctedWorkdays timeNow config = map correct workdays
    where correct (DayInMonth d isWorkday) = DayInMonth d $ fromMaybe isWorkday $ (isAdd <$> correctionForDay d)
          correctionForDay d = find (isDay d) $ Cfg.corrections config
          isAdd = (Cfg.Add == ) . Cfg.correctType
          isDay d = ((day d ==) . Cfg.day)
          workdays = workdaysInMonth timeNow

printMonthCalendar :: IOAtTime ()
printMonthCalendar = do
  timeNow <- execTime

  let printWeek w = message $ intercalate " " $ map showDay w
      slotWidth = 5
      filler w = take w $ repeat ' '
      showDay Nothing = filler slotWidth
      showDay (Just (DayInMonth start isWorking))
          | isWorking = slot $ d2 ++ " "
          | otherwise = slot $ "[" ++ d2 ++ "]"
          where d = show $ day start
                places l t = (filler (l - length t)) ++ t
                             
                slot = (slotWidth `places`) . (todayMark ++ )
                d2 = (2 `places` d)

                todayMark = if isSameDay timeNow start
                            then "*"
                            else ""

      buildWeeks days = takeWeeks $ (take correctLen $ repeat Nothing) ++ (map Just days)
          where correctLen = weekDayIndex $ dayStart $ head days
                takeWeeks [] = [[]]
                takeWeeks days = let (week, rest) = splitAt 7 days
                                 in week:(takeWeeks rest)

      weekDayIndex d = fromJust $ (weekDay d) `elemIndex` [Monday, 
                                                           Tuesday, 
                                                           Wednesday, 
                                                           Thursday, 
                                                           Friday, 
                                                           Saturday, 
                                                           Sunday]
      
  config <- Cfg.getConfig
  mapM_ printWeek $ buildWeeks $ correctedWorkdays timeNow config


minStr :: Int -> String
minStr minutes =
    printf "%s%d:%02d" sign hours restMinutes
    where hours = absMinutes `div` 60
          restMinutes = absMinutes `mod` 60
          absMinutes = abs minutes
          sign = if minutes >= 0
                 then ""
                 else "-"

l <&&> r = (&&) <$> l <*> r
l <||> r = (||) <$> l <*> r

showStats :: [Field] -> Config -> Time -> [Event] -> String
showStats fields config timeNow fullState = concatMap showField fields

    where showField (Preformat s) = s
          showField CurrentState                = currentState
          showField TodayIfLeaveNow             = minStr ifLeaveNow
          showField LeftForToday                = minStr leftForToday
          showField WeekBeforeToday             = minStr $ wasPresent $ thisWeek <&&> beforeToday
          showField MonthBeforeThisWeek         = minStr $ wasPresent $ thisMonth <&&> beforeThisWeek
          showField MonthBeforeThisWeekRequired = minStr $ timeRequired $ thisMonth <&&> beforeThisWeek
          showField MonthBeforeToday            = minStr $ workedThisMonth
          showField MonthBeforeTodayRequired    = minStr $ timeRequired $ thisMonth <&&> beforeToday
          showField MonthTimesDiff              = minStr $ workedThisMonth - requiredThisMonth
          showField AvgDayToFullWorkload        = minStr $ avgTimeAtDayToFullWorkload
          showField RecommendedLeaveTime        = recommendedLeaveTime
          showField WorkdaysInMonth             = show $ (length workdays)
          showField WorkdaysPassed              = show $ (length workdays) - workdaysLeft
          showField WorkdaysLeftIncludingToday  = show $ workdaysLeft
          showField WorkdayEndsAt               = workdayEndsAt

          ifLeaveNow = timeOf (leavesAndEnters ++ [(Leave timeNow)]) today
          leftForToday = 8 * 60 - ifLeaveNow

          workedThisMonth   = wasPresent   $ thisMonth <&&> beforeToday
          requiredThisMonth = timeRequired $ thisMonth <&&> beforeToday

          wasPresent   = timeOf leavesAndEnters
          timeRequired = timeOf (concat $ map fullWorkDay workdays)

          fullWorkDay time = [Enter time, Leave (time `addHours` 8)]

          dayEndTime t = if lastIsEnter
                         then if isToday
                              then if leaveNow
                                           then "now"
                                           else formatTime "%H:%M" t
                                      else "tomorrow"
                                 else "not entered"

              where isToday = today t
                    leaveNow = t < timeNow

          workdayEndsAt = dayEndTime $ timeNow `addMinutes` leftForToday

          todayIsWorkday = isJust $ find today workdays

          recommendedLeaveTime =
              dayEndTime $ timeNow `addMinutes` 
                             (if todayIsWorkday
                              then (avgTimeAtDayToFullWorkload + leftForToday)
                              else (requiredThisMonth - workedThisMonth - ifLeaveNow))

          currentState = if lastIsEnter
                         then "entered"
                         else "leaved"

          lastIsEnter = if null leavesAndEnters
                        then False
                        else isEnter $ last leavesAndEnters

          timeOf events condition = sum $ mapBy2 addTime $ filter (condition . evTime) events
              where addTime (Enter timeEnter) (Leave timeLeave) = timeLeave `minutesAfter` timeEnter
                    addTime _ _ = 0

          today          = isSameDay   timeNow
          thisMonth      = isSameMonth timeNow
          thisWeek       = isSameWeek  timeNow
          afterNow       = (> timeNow)
          beforeToday    = (not . today) <&&> (< timeNow)
          beforeThisWeek = (not . thisWeek) <&&> (< timeNow)

          workdays = map dayStart $ filter isWorking $ correctedWorkdays timeNow config

          workdaysLeft = 1 + (length $ filter (thisMonth <&&> afterNow) workdays)

          avgTimeAtDayToFullWorkload = (requiredThisMonth - workedThisMonth) `div` workdaysLeft

          leavesAndEnters = S.leavesAndEnters fullState
