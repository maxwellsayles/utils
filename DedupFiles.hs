{- | Print out files with matching SHA256 hashes. -}

import Control.Applicative
import Control.Exception
import Control.Monad
import Crypto.Hash.SHA256 (hashlazy)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO (hFlush, stdout)
import Text.Printf

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as S
import qualified System.Posix.Files as Posix

usage :: IO ()
usage = putStrLn "Usage: DedupFiles <path>"

handleException :: IO a -> IO a -> IO a
handleException excResult result =
  handle (\(SomeException e) -> print e >> excResult) result

listContents :: FilePath -> IO [FilePath]
listContents fp = 
  handleException (return []) $ do
    exists <- doesDirectoryExist fp
    files <- if exists then listDirectory fp else return []
    return $! if fp /= "." then map (fp </>) files else files

isRegularFile :: FilePath -> IO Bool
isRegularFile fp =
  handleException (return False) $
  (Posix.isRegularFile <$> Posix.getSymbolicLinkStatus fp)

isDirectory :: FilePath -> IO Bool
isDirectory fp =
  handleException (return False) $
  (Posix.isDirectory <$> Posix.getSymbolicLinkStatus fp)

listFiles :: FilePath -> IO [FilePath]
listFiles = filterM isRegularFile <=< listContents

listDirs :: FilePath -> IO [FilePath]
listDirs = filterM isDirectory <=< listContents

listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive path = do
  files <- listFiles path
  subDirFiles <- mapM listFilesRecursive =<< listDirs path
  return $ files ++ concat subDirFiles

hashFiles :: Int
          -> Int
          -> [(BS.ByteString, FilePath)]
          -> [FilePath]
          -> IO [(BS.ByteString, FilePath)]
hashFiles _ n acc [] = do
  printf "\x1b[2K\rDone hashing %d files.\n" n
  return acc
hashFiles i n acc (path:paths) = do
  printf "\rHashing %d / %d." i n
  hash <- hashlazy <$> BSL.readFile path
  hash `seq` hashFiles (i + 1) n ((hash, path) : acc) paths

printDuplicates :: [FilePath] -> IO ()
printDuplicates path = do
  mapM_ putStrLn path
  putStrLn ""

dedup :: String -> IO ()
dedup path = do
  files <- listFilesRecursive path
  hashes <- hashFiles 0 (length files) [] files
  let dups = filter ((> 1) . length) $
             map (map snd) $
             groupBy ((==) `on` fst) $
             sortBy (comparing fst) hashes

  mapM_ printDuplicates dups

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then usage
    else dedup $ head args
