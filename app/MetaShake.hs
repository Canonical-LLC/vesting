import Development.Shake
import Development.Shake.FilePath
import Control.Monad

main :: IO ()
main = do
  let buildDir = "dist-newstyle/build/aarch64-osx/ghc-8.10.7/vesting-1.0.0.0/x/"
      shakeFile = "app/Shake.hs"
      exeName   = "project-shake"
      shakeExe  = buildDir </> exeName </> "build" </> exeName </> exeName

  shakeArgs shakeOptions { shakeFiles = ".meta-shake" } $ do

    want [shakeExe]

    shakeExe %> \_ -> do
      need [shakeFile]
      b <- doesFileExist shakeExe
      when b $
        cmd_ shakeExe "clean"

      cmd_ "cabal" "build" exeName
