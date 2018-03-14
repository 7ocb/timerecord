module Main where

import Timerecord.ExecutionTime (runIOAtNowShowingError)
import Timerecord.IO
import Data.List (intercalate, nub)
import System.Environment (getArgs)
import Timerecord.Commons
import System.Console.Readline hiding (addHistory)
import qualified System.Console.Readline as RL
import Control.Monad (liftM2, fmap, when, join, void)

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe)

historyFile = (++ "/cmd_history") <$> timerecordsDir

history = do 
  hf <- historyFile
  (fromMaybe [] . (lines <$>)) <$> safeReadFile hf

initReadline = do
  history >>= mapM_ RL.addHistory
  
  readlineLoop

historySize = 100

addHistory :: String -> IO ()
addHistory cmd = do
  RL.addHistory cmd
  let updateHistory = reverse . take historySize . nub . reverse . filter (not . null) . (++ [cmd])

  join $ safeWriteFile <$> historyFile <*> ((unlines . updateHistory) <$> history)


readlineLoop = do
  let handleInput Nothing = putStrLn ""
      handleInput (Just command) = do result <- runIOAtNowShowingError $ process command
                                      when (result /= (Right False)) $ addHistory command
                                      readlineLoop
  join $ handleInput <$> readline "% "
    
main = do 
  args <- getArgs
  if null args
  then initReadline
  else void $ runIOAtNowShowingError $ process $ intercalate " " args
