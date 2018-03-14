{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Timerecord.ExecutionTime (ExecutionTime,
                                 QueryEnvironment,
                                 runQueryEnvironment,
                                 liftIO,
                                 execTime,
                                 throwE,
                                 catching,
                                 throwing,
                                 IOAtTime,
                                 runIOAtNow,
                                 runIOAtNowShowingError,
                                 listReportFormatFiles,
                                 getReportFormatFileContents,
                                 listStatsFormatFiles,
                                 getStatsFormatFileContents,
                                 withExecTime,
                                 message,
                                 thisMonthFile,
                                 Storeable(..),
                                 put,
                                 get,
                                 transform)
where


import Commons.Time
import Control.Monad.Reader (ReaderT, runReaderT, asks, local)
import qualified Control.Monad.State as S
import Control.Monad (liftM2)
import Control.Monad.Trans.Except hiding (throwE)
import qualified Control.Monad.Trans.Except as E
import Control.Monad.Trans (lift)
import System.FilePath ((</>))

import Data.Maybe (fromMaybe)

import qualified Timerecord.IO as TrIO

newtype IOAtTime a = IOAtTime { 
      runIOAtTime :: ExceptT String (ReaderT ExecutionTime IO) a
    } deriving (Monad, Applicative)

newtype QueryEnvironment a = QueryEnvironment {
      runQueryEnvironment :: IOAtTime a
    } deriving (Monad, Applicative, Functor)

instance Functor IOAtTime where
    fmap f = IOAtTime . fmap f . runIOAtTime

newtype ExecutionTime = ExecutionTime {
      getTime :: Time
    }

liftIO :: IO a -> IOAtTime a
liftIO = IOAtTime . lift . lift

runIOAtNow :: IOAtTime a -> IO (Either String a)
runIOAtNow action = 
    currentTime >>= runReaderT (runExceptT (runIOAtTime action)) . ExecutionTime 

runIOAtNowShowingError :: IOAtTime a -> IO (Either String a) 
runIOAtNowShowingError action = do 
  result <- runIOAtNow $ action
  case result of 
    Left msg -> putStrLn $ "Error: " ++ msg
    _ -> return ()
  return result

throwE :: String -> IOAtTime a
throwE = IOAtTime . E.throwE 

catching :: IOAtTime a -> IOAtTime (Either String a)
catching = liftIO . runIOAtNow

throwing :: Either String a -> IOAtTime a
throwing = either throwE return 

withExecTime :: Time -> IOAtTime a -> IOAtTime a
withExecTime time action = IOAtTime $ local (const $ ExecutionTime time) (runIOAtTime action)

thisMonthFile :: (String -> String) -> IOAtTime String
thisMonthFile transform = liftM2 (</>)
                          (liftIO TrIO.timerecordsDir)
                          ((transform . formatTime "%Y.%m") `fmap` execTime)

listFormats :: String -> QueryEnvironment [String]
listFormats = QueryEnvironment . liftIO . TrIO.listFormatFiles

listReportFormatFiles :: QueryEnvironment [String]
listReportFormatFiles = listFormats TrIO.reportFormats

listStatsFormatFiles :: QueryEnvironment [String]
listStatsFormatFiles = listFormats TrIO.statsFormats

formatContents :: String -> String -> IOAtTime String
formatContents dir file = do 
  result <- liftIO $ TrIO.getFormatFileContents dir file
  throwing result

getReportFormatFileContents :: String -> IOAtTime String 
getReportFormatFileContents = formatContents TrIO.reportFormats

getStatsFormatFileContents :: String -> IOAtTime String 
getStatsFormatFileContents = formatContents TrIO.statsFormats

data Storeable a = Storeable {
      defValue :: a,
      readIt :: String -> a,
      showIt :: a -> String,
      fnMod :: String -> String
    }

get :: Storeable a -> IOAtTime a
get s = do
  file <- thisMonthFile (fnMod s)

  stored <- liftIO $ TrIO.safeReadFile file

  return $ fromMaybe (defValue s) ((readIt s) `fmap` stored)

put :: Storeable a -> a -> IOAtTime ()
put s v = do
  file <- thisMonthFile (fnMod s)

  liftIO $ TrIO.safeWriteFile file $ (showIt s) v

execTime :: IOAtTime Time
execTime = IOAtTime $ asks getTime

transform :: Storeable a -> (a -> a) -> IOAtTime ()
transform s t = t `fmap` get s >>= put s

message = liftIO . putStrLn