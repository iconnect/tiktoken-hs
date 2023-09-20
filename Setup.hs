{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (SomeException, try, onException)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Distribution.Compat.Lens
import Distribution.PackageDescription hiding (buildInfo, includeDirs)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Setup hiding (Flag)
import Distribution.System
import Distribution.Verbosity
import System.Environment
import System.IO
import qualified Distribution.Types.BuildInfo.Lens as L

cargoProgram :: Program
cargoProgram = simpleProgram "cargo"

rustcProgram :: Program
rustcProgram = simpleProgram "rustc"

newtype RustTargetHost = RustTargetHost { getTargetHost :: String }
  deriving Show

-- | Calls \"rust -vV\" and extracts the target architecture out of it.
-- >>> getRustHostTarget pgm
-- "x86_64-apple-darwin"
getRustHostTarget :: ConfiguredProgram -> IO RustTargetHost
getRustHostTarget configuredProgram =
  let mkTarget = RustTargetHost . head . drop 1 . words . head . drop 4 . lines
  in mkTarget <$> getProgramOutput silent configuredProgram ["-vV"]

withConfiguredProgram :: ConfigFlags -> Program -> (ConfiguredProgram -> IO a) -> IO a
withConfiguredProgram confFlags pgm act = do
  let verbosity = fromFlag $ configVerbosity confFlags
  (foundPgm, _) <- requireProgram verbosity pgm (configPrograms confFlags)
  act foundPgm

withRustPath :: ConfigFlags -> (String -> IO a) -> IO a
withRustPath confFlags withPath = do
  withConfiguredProgram confFlags rustcProgram $ \rustc -> do
    (RustTargetHost hostTarget) <- getRustHostTarget rustc
    withPath $ rustArtifactsPath confFlags <> "/" <> hostTarget <> "/debug"

addRustPaths :: ConfigFlags -> IO ()
addRustPaths confFlags = do
  withRustPath confFlags $ \rustPath -> do
   addToPath "LD_LIBRARY_PATH" rustPath
   addToPath "DYLD_LIBRARY_PATH" rustPath
  where
    addToPath :: String -> String -> IO ()
    addToPath pathVar path = do
      v <- try $ getEnv pathVar :: IO (Either SomeException String)
      setEnv pathVar (path ++ either (const "") (":" ++) v)

rustArtifactsPath :: ConfigFlags -> FilePath
rustArtifactsPath cfg = fromFlag (configDistPref cfg) <> "/rust"

buildRustWrapper :: ConfigFlags -> IO ()
buildRustWrapper confFlags = withConfiguredProgram confFlags cargoProgram $ \cargo -> do
  let verbosity = fromFlag $ configVerbosity confFlags
  void $ getProgramOutput verbosity cargo
    ["+nightly"
    , "-Z"
    , "unstable-options"
    , "-C"
    , "tiktoken-rs-hs-wrapper"
    , "cbuild"
    , "--target-dir"
    , rustArtifactsPath confFlags
    ] `onException` (putStrLn "Make sure to install the rust nightly (\"rustup install nightly\") and the c-cargo applet (\"cargo install cargo-c\").")

extendConfigFlags :: ConfigFlags -> IO ConfigFlags
extendConfigFlags confFlags = do
  withRustPath confFlags $ \rustPath -> do
    pure $ confFlags {
           configExtraLibDirs     = rustPath : (configExtraLibDirs confFlags)
         , configExtraIncludeDirs = rustPath : (configExtraIncludeDirs confFlags)
         }

main :: IO ()
main = do
  let origUserHooks = simpleUserHooks

  defaultMainWithHooks origUserHooks {
      hookedPrograms = [ cargoProgram, rustcProgram ]

    -- Build the Rust wrapper library.
    , preConf = \args confFlags -> do
        buildRustWrapper confFlags
        preConf origUserHooks args confFlags

    -- Ensures the built library path is available for the configure script
    -- to pick it up, for foreign dependency resolution.
    , confHook = \(genPkgDescription, hookedBuildInfo) confFlags -> do
        confFlags' <- extendConfigFlags confFlags
        confHook origUserHooks (genPkgDescription, hookedBuildInfo) confFlags'

    -- Ensures that the built wrapper library is available both at
    -- LD_LIBRARY_PATH and DYLD_LIBRARY_PATH. Re-builds the Rust library.
    , testHook = \args packageDesc localBuildInfo userHooks testFlags -> do
        let confFlags = configFlags localBuildInfo
        buildRustWrapper confFlags
        addRustPaths confFlags
        testHook origUserHooks args packageDesc localBuildInfo userHooks testFlags
   }
