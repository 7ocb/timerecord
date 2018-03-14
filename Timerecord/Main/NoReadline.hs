module Main where

import Timerecord.Commons
import Data.List
import System.Environment
import Control.Monad (liftM2,
                      fmap)

import Timerecord.ExecutionTime (runIOAtNowShowingError)

main = ((concat . intersperse " ") `fmap` getArgs) >>= runIOAtNowShowingError . process
