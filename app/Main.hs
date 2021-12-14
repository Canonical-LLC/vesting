{-# LANGUAGE RecordWildCards #-}

module Main where

import Cardano.Api hiding (TxId)
import Options.Applicative

import Canonical.Vesting

data Opts = Opts
  {  output :: FilePath
  } deriving Show

main :: IO ()
main = createSC =<< execParser opts

opts :: ParserInfo Opts
opts = info (optsParser <**> helper) . mconcat $
  [ fullDesc
  , progDesc "Create a smart contract for sharing"
  ]

optsParser :: Parser Opts
optsParser = Opts
  <$> (strOption . mconcat $
    [ long "output"
    , metavar "FILE"
    , help "Where to write the script."
    ])

createSC :: Opts -> IO ()
createSC Opts{..} = do
  result <- writeFileTextEnvelope output Nothing $ vesting
  case result of
      Left err -> print $ displayError err
      Right () -> putStrLn $ "wrote validator to file " ++ output
