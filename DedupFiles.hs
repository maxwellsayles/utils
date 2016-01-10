import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.ByteString as BS
import Data.Digest.OpenSSL.MD5
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Ord
import System.Environment (getArgs)
import System.FilePath.Posix (takeFileName)
import System.Process (readProcessWithExitCode)
import System.Exit

usage :: IO ()
usage = putStrLn "Usage: DedupFiles <path>"

doit :: String -> IO ()
doit path = do
  (exitCode, out, err) <- readProcessWithExitCode "find" [path, "-type", "f"] ""
  case exitCode of
    ExitFailure _ -> putStrLn err
    ExitSuccess -> dedupPaths $ lines out

dedupPaths :: [String] -> IO ()
dedupPaths paths = do
  let dupFiles = concat $
                 filter ((>1) . length) $
                 map (map snd) $
                 groupBy ((==) `on` fst) $
                 sortBy (comparing fst) $
                 map (takeFileName &&& id) paths
  
  putStrLn $ "Computing the MD5 sum of " ++ show (length dupFiles) ++ " files."
  dupHashes <- mapM (\path -> do
                        hash <- md5sum <$> BS.readFile path
                        return $! hash) dupFiles
  let dups = filter ((>1) . length) $
             map (map snd) $
             groupBy ((==) `on` fst) $
             sortBy (comparing fst) $
             zip dupHashes dupFiles

  putStrLn ""
  forM_ dups $ \v -> mapM putStrLn v >> putStrLn ""

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 then usage else doit (head args)
