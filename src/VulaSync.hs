module VulaSync
    ( vulaSync,
      readVulaSyncConfig
    ) where

import Types (VulaM, VulaSyncConfig(..), VulaTabConfig(..))
import Shell (rsync, withGVFS)

import qualified Control.Monad.Parallel as ParM
import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString.Lazy as BS
import Data.Aeson (eitherDecode)
import Data.Either.Combinators
import Data.Maybe (fromMaybe)

import Network.URI (URI(uriScheme, uriAuthority, uriPath), URIAuth(uriUserInfo, uriRegName), uriToString, parseURI,
                    escapeURIString, isUnescapedInURIComponent)
import System.FilePath ((</>), addTrailingPathSeparator)
import System.IO (hFlush, hSetEcho, stdout, stdin)

readVulaSyncConfig :: FilePath -> VulaM VulaSyncConfig
readVulaSyncConfig path = do
  config <- liftIO (BS.readFile path)
  eitherToError $ eitherDecode config

prepareVulaURI :: String -> URI -> URI
prepareVulaURI username = updateURI
  where
    updateURI uri = uri { uriScheme = "davs:"
                        , uriAuthority = updateURIAuth <$> uriAuthority uri }
    updateURIAuth auth = auth { uriUserInfo = username ++ "@" }

getPassword :: VulaM String
getPassword = liftIO $ do
  putStr "Password: "
  hFlush stdout
  hSetEcho stdin False
  password <- getLine
  hSetEcho stdin True
  putChar '\n'
  return password

uriMountPathSuffix :: String -> URI -> String
uriMountPathSuffix username uri = concat [ "dav:"
                                , "host=", fromMaybe "" $ uriRegName <$> uriAuthority uri, ","
                                , "ssl=true,"
                                , "user=", username, ","
                                , "prefix=", escapeURIString isUnescapedInURIComponent $ uriPath uri]

vulaSync :: VulaSyncConfig -> VulaM ()
vulaSync config = do
  let username = vulaUsername config
  password <- getPassword
  ParM.forM_ (vulaTabConfigs config) $
    \tabConfig -> do
      let tabPath = vulaTabPath tabConfig
      let rawURI = vulaTabUri tabConfig
      uri <- maybe (throwError $ "Invalid URI " ++ rawURI) return (prepareVulaURI username <$> parseURI rawURI)
      let mountPath = vulaGVFSMountPathPrefix config </> uriMountPathSuffix username uri
      liftIO $ putStrLn $ "Syncing to " ++ tabPath
      withGVFS mountPath (uriToString id uri "") password
        (rsync (rSyncFlags tabConfig) (addTrailingPathSeparator mountPath) tabPath)



