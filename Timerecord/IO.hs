module Timerecord.IO
    (safeWriteFile,
     safeReadFile,
     timerecordsDir,
     statsFormats,
     reportFormats,
     listFormatFiles,
     getFormatFileContents)
where

import System.FilePath ((</>))
import System.Directory 
import Control.Exception (catch,
                          try,
                          IOException)    
import Control.Monad 

timerecordsDir :: IO String
timerecordsDir = (</> ".timerecord") `fmap` getHomeDirectory

safeWriteFile :: FilePath -> String -> IO ()
safeWriteFile file contents = do 
  let tempFile = file ++ ".temp"
                   
  writeFile tempFile contents
  renameFile tempFile file



safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile path = do
  catch (Just `fmap` readFile path) orDefault
      where orDefault :: IOException -> IO (Maybe String)
            orDefault = const $ return $ Nothing

readFileEither :: String -> IO (Either String String)
readFileEither file = do 
  exists <- doesFileExist file

  let orError :: IOException -> IO (Either String String)
      orError = return . Left . (("Can't read file '" ++ file ++ "':") ++ ) . show

  if exists
  then catch (Right `fmap` readFile file) orError
  else return $ Left $ "File '" ++ file ++ "' does not exist or can't be read"

reportFormats :: String
reportFormats = "report_formats"

statsFormats :: String
statsFormats = "stats_formats"

listFormatFiles :: String -> IO [String]
listFormatFiles dir = do
  formatsDir <- (</> dir) `fmap` timerecordsDir
      
  exists <- doesDirectoryExist formatsDir

  if exists 
  then getDirectoryContents formatsDir >>= filterM (doesFileExist . (formatsDir </>)) 
  else return []

getFormatFileContents :: String -> String -> IO (Either String String)
getFormatFileContents dir name = do
  file <- (</> dir </> name) `fmap` timerecordsDir

  readFileEither file
  

