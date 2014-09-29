module Main where

import Options.Applicative
import Web.Cloud

data Sample = Sample
  { hello :: String
  , quiet :: Bool }

sample :: Parser Sample
sample = Sample
  <$> strOption
      ( long "hello"
     <> metavar "TARGET"
     <> help "Target for the greeting" )
  <*> switch
      ( long "quiet"
     <> help "Whether to be quiet" )

greet :: Sample -> IO ()
greet (Sample h False) = putStrLn $ "Hello, " ++ h
greet _ = return ()

main :: IO ()
main = execParserWebCloud opts >>= greet
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
