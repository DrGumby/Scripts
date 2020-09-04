module ControlledVisit where

import BetterPredicate
import Control.Monad (filterM, liftM, forM)
import System.Directory (Permissions (..), getModificationTime, getPermissions, doesDirectoryExist, getDirectoryContents)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension, (</>))
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile, withFile)

data Info = Info {
    infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

errHandler :: SomeException -> IO (Maybe a)
errHandler = const (return Nothing)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle errHandler (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (withFile path ReadMode hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info ->
    if isDirectory info && infoPath info /= path
    then ControlledVisit.traverse order (infoPath info)
    else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
