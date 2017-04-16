{-# LANGUAGE RecordWildCards #-}
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Time.Clock (UTCTime)
import System.Directory
import System.FilePath
import System.Posix.Types (EpochTime)

import qualified Data.Set as S
import qualified System.Posix.Files as Posix

data Env = Env { srcPath :: FilePath
               , dstPath :: FilePath
               , dryRun :: Bool
               }

data Meta = Meta { metaPath :: String
                 , metaSize :: Integer
                 , metaATime :: EpochTime
                 , metaMTime :: EpochTime
                 }

instance Eq Meta where
  a == b = metaPath a == metaPath b &&
           metaSize a == metaSize b &&
           metaMTime a == metaMTime b

instance Ord Meta where
  a `compare` b
    | metaPath a /= metaPath b = metaPath a `compare` metaPath b
    | metaSize a /= metaSize b = metaSize a `compare` metaSize b
    | otherwise = metaMTime a `compare` metaMTime b

handleIOException :: IO a -> IO a -> IO a
handleIOException excResult result =
  handle (\(SomeException e) -> print e >> excResult) result

getMeta :: FilePath -> IO Meta
getMeta fp = do
  status <- Posix.getSymbolicLinkStatus fp
  let atime = Posix.accessTime status
      mtime = Posix.modificationTime status
  size <- getFileSize fp
  return $! Meta fp size atime mtime

listContents :: FilePath -> IO [FilePath]
listContents fp = 
  handleIOException (return []) $ do
    exists <- doesDirectoryExist fp
    files <- if exists then listDirectory fp else return []
    return $! if fp /= "." then map (fp </>) files else files

isRegularFile :: FilePath -> IO Bool
isRegularFile fp =
  handleIOException (return False) $
  (Posix.isRegularFile <$> Posix.getSymbolicLinkStatus fp)

isDirectory :: FilePath -> IO Bool
isDirectory fp =
  handleIOException (return False) $
  (Posix.isDirectory <$> Posix.getSymbolicLinkStatus fp)

listFiles :: FilePath -> IO [FilePath]
listFiles = filterM isRegularFile <=< listContents

listFilesWithMeta :: FilePath -> IO [Meta]
listFilesWithMeta = mapM getMeta <=< listFiles

listDirs :: FilePath -> IO [FilePath]
listDirs = filterM isDirectory <=< listContents

cpFile :: Env -> Meta -> IO ()
cpFile (Env {..}) meta = do
  let path = metaPath meta
      src = srcPath </> path
      dst = dstPath </> path
  putStrLn $ ("cp " ++) src
  when (not dryRun) $
    handleIOException (return ()) $ do
      -- NOTE: System.Directory.setModificationTime didn't work over SMB so we
      -- use the System.Posix.Files methods instead
      copyFile src dst
      Posix.setFileTimes dst (metaATime meta) (metaMTime meta)

rmFile :: Env -> FilePath -> IO ()
rmFile (Env {..}) path = do
  let path' = dstPath </> path
  putStrLn $ "rm " ++ path'
  when (not dryRun) $
    handleIOException (return ()) $
    removeFile path'

mkDir :: Env -> FilePath -> IO ()
mkDir (Env {..}) path = do
  let path' = dstPath </> path
  putStrLn $ "mkdir " ++ path'
  when (not dryRun) $
    handleIOException (return ()) $
    createDirectoryIfMissing False path'

rmDir :: Env -> FilePath -> IO ()
rmDir (Env {..}) path = do
  let path' = dstPath </> path
  putStrLn $ "rm -rf " ++ path'
  when (not dryRun) $
    handleIOException (return ()) $
    removePathForcibly path'

syncFiles :: Env -> FilePath -> IO ()
syncFiles env@(Env {..}) path = do
  srcMeta <- S.fromList <$>
             (withCurrentDirectory srcPath $ listFilesWithMeta path)
  dstMeta <- S.fromList <$>
             (withCurrentDirectory dstPath $ listFilesWithMeta path)

  -- cp missing files to dst
  let cpFiles = srcMeta `S.difference` dstMeta
  mapM_ (cpFile env) cpFiles

  -- rm extra files in dst
  let srcFiles = S.map metaPath srcMeta
      dstFiles = S.map metaPath dstMeta
      rmFiles = dstFiles `S.difference` srcFiles
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
      dstPath = "/var/run/user/1000/gvfs/smb-share:server=dns-325.local,share=volume_1/home"
      dryRun = False
      env = Env srcPath dstPath dryRun
      path = "max"

  exists <- doesDirectoryExist $ dstPath </> path
  when (not exists) $ mkDir env path

  syncDir env path
