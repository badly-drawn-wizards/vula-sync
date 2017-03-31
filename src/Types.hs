{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types where

import Control.Monad.Except (ExceptT, runExceptT)

import qualified Data.Aeson as A
import GHC.Generics (Generic)

type VulaM a = ExceptT String IO a

runVulaM :: VulaM a -> IO ()
runVulaM vulaM = do
  result <- runExceptT vulaM
  case result of
    Left e -> do
      putStrLn "An error occurred:"
      putStrLn e
    _ -> mempty

type RSyncFlags = String

data VulaSyncConfig = VulaSyncConfig
  { vulaTabConfigs :: [VulaTabConfig]
  , vulaGVFSMountPathPrefix :: FilePath
  , vulaUsername :: String }
  deriving (Generic, A.FromJSON)

data VulaTabConfig = VulaTabConfig
  { vulaTabUri :: String
  , vulaTabPath :: FilePath
  , rSyncFlags :: RSyncFlags}
  deriving (Generic, A.FromJSON)
