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
  srcFiles <- S.fromList <$> (withCurrentDirectory srcPath $ listFiles path)
  dstFiles <- S.fromList <$> (withCurrentDirectory dstPath $ listFiles path)

  -- cp missing files to dst
  -- TODO: Grab metadata for src + dst before taking difference
  let cpFiles = srcFiles `S.difference` dstFiles
  mapM_ (cpFile env) cpFiles

  -- rm extra files in dst
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
  let env = Env "/home/max/" "/home/max/tmp2" False
  syncDir env "tmp"
