{-# LANGUAGE RecordWildCards #-}
import Control.Applicative
import Control.Monad
import System.Directory
import System.FilePath
import System.Posix.Files

import qualified Data.Set as S

data Env = Env { srcPath :: FilePath
               , dstPath :: FilePath
               , dryRun :: Bool
               }

listContents :: FilePath -> IO [FilePath]
listContents fp = do
  exists <- doesDirectoryExist fp
  files <- if exists then listDirectory fp else return []
  return $! if fp /= "." then map (fp </>) files else files

listFiles :: FilePath -> IO [FilePath]
listFiles = listContents >=> filterM (\f -> isRegularFile <$> getFileStatus f)

listDirs :: FilePath -> IO [FilePath]
listDirs = listContents >=> filterM (\f -> isDirectory <$> getFileStatus f)

syncFiles :: Env -> FilePath -> IO ()
syncFiles (Env {..}) path = do
  srcFiles <- S.fromList <$> (withCurrentDirectory srcPath $ listFiles path)
  dstFiles <- S.fromList <$> (withCurrentDirectory dstPath $ listFiles path)

  -- cp missing files to dst
  -- TODO: Grab metadata for src + dst before taking difference
  let cpFiles = srcFiles `S.difference` dstFiles
  mapM_ (putStrLn . ("cp " ++)) $ S.map (srcPath </>) cpFiles
  -- TODO: cp with time, owner, whatever

  -- rm extra files in dst
  let rmFiles = dstFiles `S.difference` srcFiles
  mapM_ (putStrLn . ("rm " ++)) $ S.map (dstPath </>) rmFiles
  -- TODO: rm
  
syncDir :: Env -> FilePath -> IO ()
syncDir env@(Env {..}) path = do
  syncFiles env path
  
  srcDirs <- S.fromList <$> (withCurrentDirectory srcPath $ listDirs path)
  dstDirs <- S.fromList <$> (withCurrentDirectory dstPath $ listDirs path)

  -- TODO: Create any missing dirs in dst
  
  --createDirectoryIfMissing False $ dst </> path

  -- Iterate over all src dirs
  mapM_ (syncDir env) srcDirs

  -- rmdir extra dst dirs
  let rmDirs = S.map (dstPath </>) $ dstDirs `S.difference` srcDirs
  mapM_ (putStrLn . ("rm -rf " ++)) rmDirs
  -- TODO: rmdir

main :: IO ()
main = do
  let env = Env "/home/max/tmp" "/home/max/Downloads" True
  syncDir env "."
