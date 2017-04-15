{-# LANGUAGE RecordWildCards #-}
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Time.Clock (UTCTime)
import System.Directory
import System.FilePath

import qualified Data.Set as S
import qualified System.Posix.Files as Posix

data Env = Env { srcPath :: FilePath
               , dstPath :: FilePath
               , dryRun :: Bool
               }

data Meta = Meta { metaPath :: String
                 , metaSize :: Integer
                 , metaDate :: UTCTime
                 } deriving (Eq, Ord)

getMeta :: FilePath -> IO Meta
getMeta fp = Meta <$> pure fp <*> getFileSize fp <*> getModificationTime fp

listContents :: FilePath -> IO [FilePath]
listContents fp = 
  -- TODO: Report exception
  handle (\(SomeException _) -> return []) $ do
    exists <- doesDirectoryExist fp
    files <- if exists then listDirectory fp else return []
    return $! if fp /= "." then map (fp </>) files else files

isRegularFile :: FilePath -> IO Bool
isRegularFile fp =
  -- TODO: Report exception
  handle (\(SomeException _) -> return False) $
  (Posix.isRegularFile <$> Posix.getSymbolicLinkStatus fp)

isDirectory :: FilePath -> IO Bool
isDirectory fp =
  -- TODO: Report exception
  handle (\(SomeException _) -> return False) $
  (Posix.isDirectory <$> Posix.getSymbolicLinkStatus fp)

listFiles :: FilePath -> IO [FilePath]
listFiles = filterM isRegularFile <=< listContents

listFilesWithMeta :: FilePath -> IO [Meta]
listFilesWithMeta = mapM getMeta <=< listFiles

listDirs :: FilePath -> IO [FilePath]
listDirs = filterM isDirectory <=< listContents

cpFile :: Env -> FilePath -> IO ()
cpFile (Env {..}) path = do
  putStrLn $ ("cp " ++) $ srcPath </> path
  when (not dryRun) $ do
    copyFileWithMetadata (srcPath </> path) (dstPath </> path)

rmFile :: Env -> FilePath -> IO ()
rmFile (Env {..}) path = do
  let path' = dstPath </> path
  putStrLn $ "rm " ++ path'
  when (not dryRun) $ removeFile path'

mkDir :: Env -> FilePath -> IO ()
mkDir (Env {..}) path = do
  let path' = dstPath </> path
  putStrLn $ "mkdir " ++ path'
  when (not dryRun) $ createDirectoryIfMissing False path'

rmDir :: Env -> FilePath -> IO ()
rmDir (Env {..}) path = do
  let path' = dstPath </> path
  putStrLn $ "rm -rf " ++ path'
  when (not dryRun) $ removePathForcibly path'

syncFiles :: Env -> FilePath -> IO ()
syncFiles env@(Env {..}) path = do
  srcMeta <- S.fromList <$>
             (withCurrentDirectory srcPath $ listFilesWithMeta path)
  dstMeta <- S.fromList <$>
             (withCurrentDirectory dstPath $ listFilesWithMeta path)

  -- cp missing files to dst
  let cpFiles = S.map metaPath $ srcMeta `S.difference` dstMeta
  mapM_ (cpFile env) cpFiles

  -- rm extra files in dst
  let srcFiles = S.map metaPath srcMeta
  let dstFiles = S.map metaPath dstMeta
  let rmFiles = dstFiles `S.difference` srcFiles
  mapM_ (rmFile env) rmFiles
  
syncDir :: Env -> FilePath -> IO ()
syncDir env@(Env {..}) path = do
  -- cp appropriate files from src to dst
  syncFiles env path
  
  srcDirs <- S.fromList <$> (withCurrentDirectory srcPath $ listDirs path)
  dstDirs <- S.fromList <$> (withCurrentDirectory dstPath $ listDirs path)

  -- Create any missing dirs in dst
  mapM_ (mkDir env) $ srcDirs `S.difference` dstDirs

  -- Iterate over all src dirs
  mapM_ (syncDir env) srcDirs

  -- rmdir extra dst dirs
  mapM_ (rmDir env) $ dstDirs `S.difference` srcDirs

main :: IO ()
main = do
  let srcPath = "/home"
      dstPath = "/home/tmp"
      dryRun = False
      env = Env srcPath dstPath dryRun
      path = "max"

  exists <- doesDirectoryExist $ dstPath </> path
  when (not exists) $ mkDir env path

  syncDir env path
