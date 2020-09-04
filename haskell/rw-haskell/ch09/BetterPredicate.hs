module BetterPredicate where

import Control.Monad (filterM)
import System.Directory (Permissions (..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile, withFile)

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath
               -> Permissions
               -> Maybe Integer
               -> UTCTime
               -> Bool

getFileSizeErrHandler :: SomeException -> IO (Maybe Integer)
getFileSizeErrHandler = const (return Nothing)

getFileSize' :: FilePath -> IO (Maybe Integer)
getFileSize' path = handle getFileSizeErrHandler $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle getFileSizeErrHandler $
  withFile path ReadMode $ \h -> do
    size <- hFileSize h
    return (Just size)

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms <- getPermissions name
          size  <- getFileSize name
          modified <- getModificationTime name
          return (p name perms size modified)

type InfoP a = FilePath
            -> Permissions
            -> Maybe Integer
            -> UTCTime
            -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP = liftP2 (||)

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP

(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP

(||?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(||?) = orP

(<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(<?) = lesserP

(>?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) = greaterP

infix 4 ==?
infixr 3 &&?
infixr 3 ||?
infix 4 >?
infix 4 <?
