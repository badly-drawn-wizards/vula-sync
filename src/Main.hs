module Main where

import VulaSync
import Types

import Options.Applicative (Parser, execParser, strOption, long, short, metavar, value, showDefault, help,
                            ParserInfo, info, helper, fullDesc, progDesc)
import System.Environment (getEnv)

import Control.Monad.Trans (liftIO)
import Control.Applicative ((<**>))

import Data.Monoid ((<>))
import System.FilePath ((</>))

main :: IO ()
main = runVulaM $ do
  home <- liftIO $ getEnv "HOME"
  vulaSyncConfig <- readVulaSyncConfig =<< liftIO (execParser $ opts home)
  vulaSync vulaSyncConfig

opts :: FilePath -> ParserInfo String
opts home = info (configParser home <**> helper) (fullDesc <> progDesc "Synchronize vula resources with local directories")

configParser :: FilePath -> Parser String
configParser home = strOption
  ( long "config"
  <> short 'c'
  <> metavar "FILE"
  <> value (home </> ".config" </> "vula-sync.json")
  <> showDefault
  <> help "The configuration file to load" )
