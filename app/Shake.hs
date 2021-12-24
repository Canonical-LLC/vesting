import Development.Shake
import Development.Shake.FilePath
import System.Environment
-- import System.Directory
-- import System.Directory.Internal.Prelude
-- import Control.Exception

main :: IO ()
main = do
  let buildDir = "dist-newstyle/build/aarch64-osx/ghc-8.10.7/vesting-1.0.0.0/x/"
  let outputDir = "output"
  let haskellSrc = ["src/Canonical/Vesting.hs"]
  Just blockChainFolder <- lookupEnv "BLOCKCHAIN_PREFIX"
  Just blockChainFlag   <- lookupEnv "BLOCKCHAIN"
  let cabalFile = "vesting.cabal"
      cabalProjectFile = "cabal.project"
      freezeFile = "cabal.project.freeze"
      plutusFile = outputDir </> "vesting.plutus"
      blockChainOutput = outputDir </> blockChainFolder
      plutusAddr = blockChainOutput </> "vesting.addr"
      createScExeName = "create-sc"
      createScExe = buildDir </> createScExeName </> "build" </> createScExeName </> createScExeName
  shakeArgs shakeOptions $ do

    want [plutusAddr]

    phony "clean" $ do
      liftIO $ removeFiles "output" ["*"]
      liftIO $ removeFiles buildDir [createScExeName]

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
      need $ haskellSrc <> [cabalFile, cabalProjectFile, freezeFile]
      cmd "cabal build"
