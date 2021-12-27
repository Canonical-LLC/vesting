{-# LANGUAGE NumericUnderscores #-}
import Development.Shake
import Development.Shake.FilePath
import System.Environment
import qualified Data.ByteString.Lazy as LBS
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Ada as Ada
import qualified Data.Aeson as Aeson
import Cardano.Api.Shelley
import Data.Time
import System.Directory
import Data.String
import Data.Time.Clock.POSIX

import qualified Canonical.Vesting as Vesting

writeDatum :: Maybe FilePath -> Plutus.PubKeyHash -> Vesting.Schedule -> IO ()
writeDatum output beneficiary schedule = write . plutusDataToJSON $ datum where
  write = maybe LBS.putStr LBS.writeFile output
  datum = Vesting.Datum beneficiary schedule

plutusDataToJSON :: Plutus.ToData a => a -> LBS.ByteString
plutusDataToJSON
  = Aeson.encode
  . (scriptDataToJson ScriptDataJsonDetailedSchema)
  . fromPlutusData
  . Plutus.toData

readPkhFile :: FilePath -> IO Plutus.PubKeyHash
readPkhFile filePath = do
  [pubKeyHashStr] <- lines <$> readFile filePath
  pure $ fromString pubKeyHashStr


main :: IO ()
main = do
  let buildDir = "dist-newstyle/build/aarch64-osx/ghc-8.10.7/vesting-1.0.0.0/x/"
  let outputDir = "output"
  let haskellSrc = ["src/Canonical/Vesting.hs"]
  Just blockChainFolder <- lookupEnv "BLOCKCHAIN_PREFIX"
  Just blockChainFlag   <- lookupEnv "BLOCKCHAIN"
  homeDir <- getHomeDirectory
  let cabalFile = "vesting.cabal"
      cabalProjectFile = "cabal.project"
      plutusFile = outputDir </> "vesting.plutus"
      blockChainOutput = outputDir </> blockChainFolder
      plutusAddr = blockChainOutput </> "vesting.addr"
      createScExeName = "create-sc"
      createScExe = buildDir </> createScExeName </> "build" </> createScExeName </> createScExeName
      applicationPrefix = "vesting"
      users = ["benefactor", "beneficiary"]
      (walletPkhs, beneficiaryPkh) =
        case map (\x -> blockChainOutput </> x <.> "pkh") users of
          xs@[_, y] -> (xs, y)
          _ -> error "Impossible happened!"
      walletAddrs = map (\x -> blockChainOutput </> x <.> "addr") users
      datumFilePath = blockChainOutput </> "datumFile.json"
      datumHashFilePath = blockChainOutput </> "datumFile-hash.txt"

  createDirectoryIfMissing True blockChainOutput

  shakeArgs shakeOptions $ do

    want ["tests"]

    phony "clean" $ do
      liftIO $ removeFiles "output" ["*"]
      liftIO $ removeFiles buildDir [createScExeName]

    "//*.addr" %> \walletAddress -> do
      let username = takeBaseName walletAddress
          verificationKey
            =   homeDir
            </> applicationPrefix
            </> username <.> "vkey"
      need [verificationKey]
      cmd_ "cardano-cli address build"
        blockChainFlag
        "--payment-verification-key-file"
        verificationKey
        "--out-file"
        walletAddress

    "//*.vkey" %> \verificationKey -> do
      let signingKey = takeExtension verificationKey <.> "skey"

      cmd_ "cardano-cli address key-gen --verification-key-file"
        verificationKey
        "--signing-key-file"
        signingKey

    "//*.pkh" %> \out -> do
      let username = takeBaseName out
          verificationKey
            =   homeDir
            </> applicationPrefix
            </> username <.> "vkey"
          pkhFilePath
            =   blockChainOutput
            </> username
            <.> "pkh"

      need [verificationKey]

      Stdout pkhOutput <- cmd "cardano-cli address key-hash --payment-verification-key-file"
        verificationKey
      liftIO $ writeFile pkhFilePath pkhOutput

    phony "test-datums" $ do
      need $ [plutusAddr] <> walletPkhs <> walletAddrs

      now <- liftIO getCurrentTime
      let
        expiration0 = Plutus.POSIXTime $ floor $ utcTimeToPOSIXSeconds $ 120 `addUTCTime` now
        expiration1 = Plutus.POSIXTime $ floor $ utcTimeToPOSIXSeconds $ 240 `addUTCTime` now

        testSchedule =
            [ Vesting.Portion expiration0 (Ada.lovelaceValueOf 1_000_000)
            , Vesting.Portion expiration1 (Ada.lovelaceValueOf 2_000_000)
            ]

      beneficiaryPkhValue <- liftIO $ readPkhFile beneficiaryPkh

      liftIO $ writeDatum (Just datumFilePath) beneficiaryPkhValue testSchedule
      Stdout out <- cmd
        "cardano-cli transaction hash-script-data --script-data-file"
        datumFilePath
      liftIO $ writeFile datumHashFilePath out


    phony "tests" $ do
      -- TODO call a test library
      need ["test-datums"]
      return ()

    plutusAddr %> \_ -> do
        need [plutusFile]

        cmd_ "cardano-cli address build"
          "--payment-script-file"
          plutusFile
          blockChainFlag
          "--out-file"
          plutusAddr

    plutusFile %> \_ -> do
      need [createScExe]
      cmd createScExe "write" "--output" plutusFile

    createScExe %> \_ -> do
      -- TODO cabal build
      need $ haskellSrc <> [cabalFile, cabalProjectFile]
      cmd "cabal build"
