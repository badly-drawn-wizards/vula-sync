module Shell where

import Types (VulaM, RSyncFlags)

import System.Process (CreateProcess, readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Directory (doesPathExist)

import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)
import Control.Monad.Catch (bracket_)
import Control.Monad (void, when, unless)

readCreateProcessEither :: CreateProcess -> String -> VulaM String
readCreateProcessEither createProcess input = do
  liftIO $ putStrLn $ "Running " ++ show createProcess
  (exitCode, out, err) <- liftIO $ readCreateProcessWithExitCode createProcess input
  case exitCode of
    ExitSuccess -> return out
    ExitFailure _ -> throwError $ unlines
      [ "Error when running '" ++ show createProcess ++ "'"
      , "STDERR:"
      , err
      , "STDOUT:"
      , out ]
rsync :: RSyncFlags -> FilePath -> FilePath -> VulaM String
rsync flags from to = do
  liftIO $ putStrLn $ "Syncing from '" ++ from ++ "' to '" ++ to ++ "'"
  result <- readCreateProcessEither (shell $ unwords ["rsync", flags, from, to]) ""
  liftIO $ putStrLn $ "Done syncing from '" ++ from ++ "' to '" ++ to ++ "'"
  return result

withGVFS :: FilePath -> String -> String -> VulaM a -> VulaM a
withGVFS mountPath uri password block = do
  let gvfs args = void $ readCreateProcessEither (shell $ unwords $ "gvfs-mount" : args) password
  alreadyMounted <- liftIO $ do
    putStrLn $ "Expecting mount location at " ++ mountPath
    doesPathExist mountPath
  when alreadyMounted $ liftIO $ putStrLn $ "Already mounted WebDAV at " ++ uri

  let withGVFS' :: VulaM a -> VulaM a
      withGVFS' = if alreadyMounted
        then id
        else (\a b c -> do {a; result <- c; b; return result}) -- TODO, get bracket_ to work here
          (do
            liftIO $ putStrLn $ "Mounting " ++ uri ++ " to " ++ mountPath
            gvfs [uri]
            liftIO $ doesPathExist mountPath >>= flip unless (liftIO $ putStrLn "Is now mounted")
          )
          (do
            liftIO $ putStrLn $ "Unmounting " ++ uri
            gvfs ["-u", uri])
  withGVFS' block
