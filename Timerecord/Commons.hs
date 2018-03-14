{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Timerecord.Commons (getCurrentMonthState,
                           currentTime,
                           process)
    where

import qualified Timerecord.IO as TrIO

import Timerecord.Config (Config, DayCorrection)
import qualified Timerecord.Config as Cfg

import Timerecord.ExecutionTime hiding (liftIO)
import qualified Timerecord.ExecutionTime as IOAtTime

import Timerecord.State
import Timerecord.Reports
import Timerecord.Util (mapBy2)

import Text.Printf (printf)
import Timerecord.ShowStats

import Timerecord.Templates
import Timerecord.Parsing
import Timerecord.ProjectCodes

import Control.Applicative ((<$>), (*>), (<*), (<*>))
import System.FilePath ((</>))

import Commons.Time

import Text.Parsec (digit, anyChar, choice, option, try, oneOf, many1, many, char, (<?>), (<|>), parserFail, space, eof, optional, sepBy, sepBy1)
import qualified Text.Parsec as Parsec
import Data.Function (on)
import Data.Either (either)
import Data.Maybe (fromMaybe, fromJust, catMaybes, isNothing)
import Data.List (isPrefixOf, find, dropWhileEnd, intersperse, intercalate, sortBy, tails, inits, groupBy, elemIndex, partition)

import Control.Monad (liftM2, liftM, fmap, when, unless, void)
import Control.Monad.Reader (ReaderT, runReaderT, liftIO, ask, asks)

joinBy = intercalate

versionMajor = 4
versionMinor = 3

printStatisticsWith :: (Time -> State -> String) -> IOAtTime ()
printStatisticsWith formatter =
    liftM2 formatter execTime (getCurrentMonthState) >>= message

stripString :: String -> String
stripString =  stripFront . stripEnd
    where isSpace = (== ' ')

          stripFront = dropWhile isSpace
          stripEnd = dropWhileEnd isSpace

data Help = Help { 
      helpSummary :: String, 
      helpFull :: QueryEnvironment [String] 
    }

data Command = Command {
      cmdPrefix :: String,
      cmdHelp    :: Help,
      cmdHandler :: String -> IOAtTime ()
    }

commandWithArg :: String -> Help -> (String -> Either String a) -> (a -> IOAtTime()) -> Command
commandWithArg key help parseArg handler =
    Command key help $ \arg -> (throwing $ parseArg $ stripString arg) >>= handler

commandNoArg :: String -> Help -> IOAtTime () -> Command
commandNoArg key help = Command key help . const

simpleHelp short full = Help short $ return full

examples = ("Examples:" :) . map ("  " ++ )
subcommands = ("Accepted commands:" :) . map ((" - " ++ ) . cmdPrefix)

data ReportFormatEntry = ReportFormatEntry String ReportFormat String

instance TemplateEntry ReportFormat where
    teString = ReportTaskString

instance TemplateElement ReportFormatEntry ReportFormat where
    teElement (ReportFormatEntry _ v _) = v
    teKey (ReportFormatEntry k _ _) = k
    teDescription (ReportFormatEntry _ _ d) = d

getProjectCodesMatches :: IO [Matcher]
getProjectCodesMatches = do
  configDir <- TrIO.timerecordsDir

  contents <- TrIO.safeReadFile $ configDir </> "project.codes"

  case parse "project codes" matchingFile (fromMaybe "" contents) of
    Left err -> do 
      putStrLn $ "Error: " ++ (show err)
      return []
                
    Right matchers -> return matchers
  

commandVersion = commandNoArg "version"

                 (simpleHelp "Show verison info"
                  (["This command show version info."]
                   ++ examples ["version"]))

                 (message $ "Version: " ++ show versionMajor ++ "." ++ show versionMinor ++ "\n" ++ "Storage version: " ++ show versionStorage)

commandEnter = registerEvent "enter" Enter
commandLeave = registerEvent "leave" Leave

commandAbsent = commandWithArg "absent"
                (simpleHelp "Register absence of user (time HH:MM or MM)"
                     (["This command allows to register absense. It accepts time",
                       "in two formats:",
                       "  1. HH:MM - to specify hours and minutes of absense,",
                       "     as time interval.",
                       "  2. MM - to specify only minutes of absense. This allows",
                       "     to specify more than 59 minutes, in which case it will",
                       "     internally be converted in hours and minutes."]
                      ++ examples ["absent 10",
                                   "absent 65",
                                   "absent 1:20",
                                   "absent 1:4"]))
                parseMinutes

                (\minutes -> do
                       now <- execTime

                       transformCurrentState $ (++ [Leave $ minutes `minutesInPast` now,
                                                    Enter now]))

defaultStatsFormat :: [String]
defaultStatsFormat = map statsLine defaultStats
    where statsLine (StatsEntry key name _ _) = name ++ ": " ++ key

commandStats = Command "stats"
                    (Help "Show current time stats"
                     $ do 
                       formats <- listStatsFormatFiles

                       return $ ["This command will show current time stats.",
                                 "Without argument, this command will display stats formatted",
                                 "according to template, which stored in file '" ++ defaultFile ++ "' in ",
                                 "~/.timerecord/" ++ TrIO.statsFormats,
                                 "or, if there is no such file, this format will be used as default:"]
                                 ++ defaultStatsFormat
                                 ++ ["it can be copied to '" ++ defaultFile ++ "' file for tuning.",
                                     "Also name of the other format file located in the formats",
                                     "directory can be specified as argument to this command to",
                                     "show stats in other format.",
                                     "This can be used to display only important stats by default",
                                     "and display extended stats by copying defaults format to 'full'",
                                     "stats template file and doing 'stats full'.",
                                     "Currently there is next formats in the formats directory:"]
                                 ++ map (" " ++) formats
                                 ++ ["Next keys can be used in format file:"]
                                 ++ templateHelp defaultStats
                                 ++ examples (["stats", 
                                              "stats <format>"]
                                              ++ map ("stats " ++) formats))

                    (\formatName -> do

                       formatFileContents
                           <- if null formatName
                              then do formatRead <- catching $ getStatsFormatFileContents defaultFile
                                      case formatRead of 
                                        Left _ -> defaultFormat
                                        Right format -> return format
                              else getStatsFormatFileContents formatName
                                               

                       format <- throwing $ parse "stats format" (templateParser defaultStats) formatFileContents

                       config <- Cfg.getConfig
                       
                       printStatisticsWith $ showStats format config)

    where defaultFile = "default"
          defaultFormat = return $ joinBy "\n" defaultStatsFormat
                    -- (const $ printCurrentStatistics)

commandReport = commandWithArg "report"
                (Help "Allows to display report on saved tasks."
                 $ do
                   formats <- listReportFormatFiles

                   return $ ["This command allows to display report",
                             "basing on saved tasks and predefined report formats.",
                             "This command can be used to dispay day and month reports.",
                             "Syntax is following:",
                             "  report <m/d> <format>",
                             "where",
                             " - m is key for month report. When specified, this",
                             "     will cause all month tasks be reported",
                             " - d is key for day report. When specified, this",
                             "     will cause only current day tasks be reported",
                             "format is the name of file placed in the directory",
                             "~/.timerecord/" ++ TrIO.reportFormats,
                             "which currenty contains next format files:"]
                              ++ map (" " ++) formats
                              ++ templateHelp reportFormats
                              ++ examples (["report d <format>",
                                            "report m <format>"]
                                           ++ zipWith 
                                               (\dm format -> "report " ++ dm ++ " " ++ format)
                                               (cycle ["d", "m"])
                                               formats))
                (parse "m/d" $ do 
                  many space
                  limit <- choice [char 'm' >> return isSameMonth,
                                   char 'd' >> return isSameDay]
                  many1 space 
                  format <- many anyChar
                  return (limit, format))

                (\(limit, formatFile) -> do 

                   formatFileContents <- getReportFormatFileContents formatFile
                       
                   format <- throwing $ parse "report format" (templateParser reportFormats) formatFileContents

                   codesMatchers <- IOAtTime.liftIO getProjectCodesMatches 

                   let selectProjectCode task = fromMaybe "" $ do 
                         msg <- taskMessage task
                         match codesMatchers msg

                   printStatisticsWith $ showReportWith format limit selectProjectCode)


eventKeys = [1..]
todayAndOtherEvents now = partition (isSameDay now . evTime) 

onIndexedEvents :: ([(Int, Event)] -> [(Int, Event)]) -> [Event] -> [Event]
onIndexedEvents f = map snd . f . zip eventKeys

commandMove = commandWithArg "move"
                      (let eventsCommand = cmdPrefix commandEvents
                       in simpleHelp "Move event to other time."
                                     (["This command allows to move event to other time.",
                                       "As arguments specify number of event as in " ++ eventsCommand,
                                       "command output, and a target time for event.",
                                       "Several pairs accepted.",
                                       "  Note: after each deletion order of events is changed,",
                                       "  so, please, be sure to run " ++ eventsCommand ++ " before",
                                       "  each moving."]
                                     ++ examples ["move 1 12:20",
                                                  "move 2 12:20 4 10:10"]))
                      (let moveSpecification = (,) <$> (parseNumber <* spaces) <*> parseHM
                           spaces = (many1 space)
                       in parse "move specifications" $ (moveSpecification `sepBy1` spaces) <* eof)

                      $ \moves -> do 
                        now <- execTime

                        transformCurrentState $ \events -> 
                            let (todays, others) = todayAndOtherEvents now events
                                dayStart = midnight now
                                modifyTime original@(index, event) 
                                    = case lookup index moves of
                                        Nothing -> original
                                        Just offsetInDay -> (index, moveToTime event $ dayStart `addMinutes` offsetInDay)

                            in others ++ (onIndexedEvents (map modifyTime) todays)
                        

commandDelete = commandWithArg "delete" 
                (let eventsCommand = cmdPrefix commandEvents
                 in simpleHelp "Delete registered events"
                        (["This command allows to delete one or more registered events.",
                          "Events specified as numbers from " ++ eventsCommand ++ " command output.",
                          "Several events can be specified.",
                          "  Note: after each deletion order of events is changed,",
                          "  so, please, be sure to run " ++ eventsCommand ++ " before",
                          "  each deletion."]
                         ++ examples ["delete 1",
                                                    "delete 2 3 4"]))
                (parse "indices" $ (parseNumber `sepBy1` (many1 space)) <* eof)

                $ \indicesToDelete -> do 
                  now <- execTime

                  let deleteUnneeded = onIndexedEvents $ filter (not . (`elem` indicesToDelete). fst)

                  transformCurrentState $ \ events -> 
                      let (todaysEvents, other) = todayAndOtherEvents now events
                      in other ++ deleteUnneeded todaysEvents
                

commandEvents = commandNoArg "events"
                (simpleHelp "Show current day's registered events"
                                (["This command displays events registered at current day.",
                                  "Events displayed as numbered log.",
                                  "The numbers of events can be used for the commands",
                                  cmdPrefix commandMove ++ " and " ++ cmdPrefix commandDelete ++ "."]
                                 ++ examples ["events"]))
                $ do state <- getCurrentMonthState
                     now <- execTime

                     let thisDayState = filter (isSameDay now . evTime) state
                         showEvent e = (formatTime "%H:%M" $ evTime e) ++ " | " ++ evName e ++ didMessage e
                         didMessage (DidTask msg _) = ": " ++ msg
                         didMessage _ = ""

                     mapM_ message $ zipWith (\i e -> show i ++ ". " ++ showEvent e) [1..] thisDayState
                                      

commandDid = Command "did"
                    (simpleHelp "Register task worked on" 
                     (["This command is useful for logging task worked on.",
                       "Basing on tasks, that specified with this command,",
                       "reports are built."]
                      ++ examples ["did some important task"]))
                  (\task -> (DidTask task) `liftM` execTime >>= register)

commandDays = commandWithArg "days"
                    (simpleHelp "Show and configure working days in month"
                     (["This command allows to configure working days in a month.",
                       "It is useful in case if working schedule differs from",
                       "default calendar schedule.",
                       "By default, timerecord will threat any weekday as working day",
                       "and sunday and saturday day as non-working day.",
                       "To see calendar of current month, use without agument.",
                       "To configure days, use day number prefixed with + or -",
                       "for adding and removing, respectively. Several arguments",
                       "for one command is accepted.",
                       "Additionally 'clear' can be specified for clearing config."]
                      ++ examples ["days",
                                   "days +20",
                                   "days -10",
                                   "days -10 +20 -2 -3",
                                   "days clear"]))
                    parseDaysModifications
                    processDaysCommand

commandAtday = Command "atday"
                    (simpleHelp "Execute command at other day"
                     (["This command allows to execute command as if it were run",
                       "at different day.",
                       "Day is specified as day of current month."]
                      ++ (subcommands commandsAllowedForAtday)
                      ++ ["See also command 'attime'"]
                      ++ examples ["atday 10 dayreport",
                                   "atday 12 leave 17:00",
                                   "atday 12 attime 17:00 absent 20"]))
                    processAtdayCommand

commandAttime = Command "attime"
                    (simpleHelp "Execute command at other time"
                     (["This command allows to execute command as if it were run",
                       "at different time.",
                       "Time is specified as HH:MM."]
                      ++ (subcommands commandsAllowedForAttime)
                      ++ ["It is useful in pair with 'atday'."]
                      ++ examples ["attime 12:00 absent 20",
                                   "attime 20:20 did something useful",
                                   "atday 10 attime 20:20 did something useful"]))
                    processAttimeCommand

commandHelp = Command "help"
                        (simpleHelp "Show commands help, help [command] for command help" 
                         (["This command shows help. Without arguments it shows",
                           "summary help on commands, with argument it can be used",
                           "to show help for specific command."]
                          ++ examples ["help",
                                       "help did",
                                       "help help"]))

                    showHelp

registerEvent :: String -> (Time -> Event) -> Command
registerEvent command constructor =
    Command  command
            (simpleHelp ("Register " ++ command)
             (["Without argument, register " ++ command ++ " at current time.",
               "Specify time in format HH:MM for using specific time"]
              ++ examples [command,
                           command ++ " 12:00",
                           command ++ " 9:20"]))
            (\args -> do
               let addEvent = register . constructor
               now <- execTime

               case args of
                 [] -> addEvent now
                 argString -> do mins <- throwing $ parse "HH:MM" (parseHM <* eof) argString
                                 addEvent $ (midnight now) `addMinutes` mins )

allCommands :: [Command]
allCommands = [commandEnter,
               commandLeave,
               commandAbsent,
               commandStats,
               commandReport,
               commandDid,
               commandEvents,
               commandDelete,
               commandMove,
               commandDays,
               commandAtday,
               commandAttime,
               commandHelp,
               commandVersion]

commandsAllowedForAtday :: [Command]
commandsAllowedForAtday = [commandEnter,
                           commandLeave,
                           commandEvents,
                           commandDelete,
                           commandMove,
                           commandReport,
                           commandAttime]

commandsAllowedForAttime :: [Command]
commandsAllowedForAttime = [commandAbsent,
                            commandDid]


atOtherTimeCommand :: String -> Parser a -> (Time -> a -> Either String Time) -> [Command] -> String -> IOAtTime ()
atOtherTimeCommand timeArgName timeParser timeModifier commands arg = do
  let clParser = ((,) <$> (many space *> timeParser <* many space) <*> many anyChar)
  
  (timeArg, restCl) <- throwing $ parse timeArgName clParser arg

  timeNow <- execTime

  targetTime <- throwing $ timeModifier timeNow timeArg

  case findCommand restCl commands of 
    Right (command, commandCl) -> withExecTime targetTime $ (cmdHandler command) commandCl
    Left error -> throwE $ case findCommand restCl allCommands of
                             Right (command, _) -> "Command " ++ (cmdPrefix command) ++ " is not allowed"
                             Left _ -> error

processAtdayCommand :: String -> IOAtTime ()
processAtdayCommand = atOtherTimeCommand "day" parseNumber modifyTime commandsAllowedForAtday
    where modifyTime timeNow day 
              | daysCount >= day = Right $ timeNow `withDay` day 
              | otherwise = Left $ "There is no day " ++ show day ++ " in this month (" ++ show daysCount ++ " days at all)"
              where daysCount = length $ workdaysInMonth timeNow


processAttimeCommand :: String -> IOAtTime ()
processAttimeCommand = atOtherTimeCommand "HH:MM" (toTime <$> parseHM) modifyTime commandsAllowedForAttime
    where toTime t = (t `div` 60, t `mod` 60)

          modifyTime timeNow (h, m) 
                     | modified `isSameDay` timeNow = Right modified
                     | otherwise = Left $ "atday shift makes time out of day"
                     where modified = timeNow `withHour` h `withMinute` m


data DayCommand = Clear
                | Show
                | Update [DayCorrection]
                  deriving (Show)

parseDaysModifications :: String -> Either String DayCommand
parseDaysModifications input =
    case stripString input of
      "" -> Right Show
      "clear" -> Right Clear
      stripped -> Update <$> parse "day modification" parser stripped

      where parser = ((flip Cfg.DayCorrection) <$> correctionType <*> parseNumber) `sepBy` many space
            correctionType = choice [char '+' >> return Cfg.Add,
                                     char '-' >> return Cfg.Remove]

processDaysCommand :: DayCommand -> IOAtTime ()
processDaysCommand Clear = Cfg.transformConfig $ \c -> c { Cfg.corrections = [] } 
processDaysCommand (Update updates) = Cfg.transformConfig $ Cfg.correctDays updates
processDaysCommand Show = 
  message ("Working, [non-working] and *today at this month: ") >> printMonthCalendar 



findCommand :: String -> [Command] -> Either String (Command, String)
findCommand commandLine = toResult . find ((command == ). cmdPrefix) 
    where (command, rest) = break (== ' ') $ stripString commandLine
          toResult Nothing  = Left $ "Can't find command " ++ command
          toResult (Just c) = Right $ (c, stripString rest)

process :: String -> IOAtTime Bool
process commandLine 
        | null stripped = return False
        | otherwise = do 
                (command, restCl) <- throwing $ findCommand stripped allCommands

                (cmdHandler command) restCl

                return True

        where stripped = stripString commandLine

showHelp :: String -> IOAtTime ()
showHelp rawCl = do
  let cl = stripString rawCl
      showShort c = (cmdPrefix c) ++ " - " ++ (helpSummary $ cmdHelp $ c)
                
  if null cl 
  then message $ joinBy "\n" $ map showShort allCommands
  else do 
    (command, _) <- throwing $ findCommand cl allCommands

    (runQueryEnvironment . helpFull . cmdHelp) command >>= message . joinBy "\n" 


minutesToTimeString minutes =
    printf "%s%d:%02d" sign hours restMinutes
    where hours = absMinutes `div` 60
          restMinutes = absMinutes `mod` 60
          absMinutes = abs minutes
          sign = if minutes >= 0
                 then ""
                 else "-"

data StatsEntry = StatsEntry String String Field String

instance TemplateEntry Field where
    teString = Preformat

instance TemplateElement StatsEntry Field where 
    teElement (StatsEntry _ _ f _) = f
    teDescription (StatsEntry _ _ _ d) = d
    teKey (StatsEntry k _ _ _) = k

defaultStats :: [StatsEntry]
defaultStats = [StatsEntry "{STATE_NOW}"
                           "Currently" 
                           CurrentState
                           "Current state, entered or leaved.",
                StatsEntry "{TODAY_IF_LEAVE_NOW}"
                           "today, if leave now" 
                           TodayIfLeaveNow
                           "Time worked today, if leave right now.",
                StatsEntry "{LEFT_FOR_TODAY_8H}"
                           "left for today" 
                           LeftForToday
                           "Time left to work today: 8h - worked today.",
                StatsEntry "{THIS_WEEK_BEFORE_TODAY}"
                           "week before today" 
                           WeekBeforeToday
                           "Worked during this week but not counting today.",
                StatsEntry "{THIS_MONTH_BEFORE_THIS_WEEK}"
                           "month before this week" 
                           MonthBeforeThisWeek
                           "Worked during this month but not counting current week.",
                StatsEntry "{REQUIRED_THIS_MONTH_BEFORE_THIS_WEEK}"
                           "month before this week required"
                           MonthBeforeThisWeekRequired
                           "Required worked time during this month not counting current week.",
                StatsEntry "{MONTH_BEFORE_TODAY}"
                           "month before today" 
                           MonthBeforeToday
                           "Worked this month not counting today.",
                StatsEntry "{REQUIRED_MONTH_BEFORE_TODAY}"
                           "month before today required" 
                           MonthBeforeTodayRequired
                           "Work required this month not counting today.",
                StatsEntry "{MONTH_OVERWORK}"
                           "month overwork (worked - required)" 
                           MonthTimesDiff
                           "Time of extra work at this month not counting today.",
                StatsEntry "{WORKDAYS_IN_MONTH}"
                           "workdays in month" 
                           WorkdaysInMonth
                           "Count of days expected to work in month.",
                StatsEntry "{WORKDAYS_LEFT}"
                           "workdays left (+today)" 
                           WorkdaysLeftIncludingToday
                           "Left days till end of month, counting today.",
                StatsEntry "{DAY_DIFF_TO_FULL_WORKLOAD}"
                           "avg day diff to full workload" 
                           AvgDayToFullWorkload
                           "Average daily overwork required to remove underwork to the end of month.",
                StatsEntry "{8H_DAY_ENDS_AT}"
                           "8h workday ends at" 
                           WorkdayEndsAt
                           "Time at which ends 8 hour workday.",
                StatsEntry "{RECOMMENDED_LEAVE_TIME}"
                           "recommented leave time" 
                           RecommendedLeaveTime
                           "Time to leave today with respect to removing underwork."]

reportFormats :: [ReportFormatEntry]
reportFormats = [ReportFormatEntry "{DD}"          ReportTaskDD          "Day in the month task worked.",
                 ReportFormatEntry "{MM}"          ReportTaskMM          "Month task worked.",
                 ReportFormatEntry "{YYYY}"        ReportTaskYYYY        "Year task worked.",
                 ReportFormatEntry "{PROJECTCODE}" ReportTaskProjectCode "Year task worked.",
                 ReportFormatEntry "{NAME}"        ReportTaskDescription "Name (description) of the task.",
                 ReportFormatEntry "{TIME}"        ReportTaskTime        "Time spent on task in the day."]



