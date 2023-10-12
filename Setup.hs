{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (SomeException, try, onException)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String
import Distribution.Compat.Lens
import Distribution.PackageDescription hiding (buildInfo, includeDirs)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.Setup hiding (Flag)
import Distribution.System
import Distribution.Verbosity
import System.Directory
import System.Environment
import System.IO
import qualified Distribution.Types.BuildInfo.Lens as L

cargoProgram :: Program
cargoProgram = simpleProgram "cargo"

rustcProgram :: Program
rustcProgram = simpleProgram "rustc"

newtype RustTargetHost = RustTargetHost { getTargetHost :: String }
  deriving Show

newtype BuildDir = BuildDir { _BuildDir :: String }
  deriving (Show, Eq, IsString)

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

withRustPath :: ConfigFlags -> BuildDir -> (String -> IO a) -> IO a
withRustPath confFlags bdir withPath = do
  withConfiguredProgram confFlags rustcProgram $ \rustc -> do
    (RustTargetHost hostTarget) <- getRustHostTarget rustc
    pth <- rustArtifactsPath bdir
    withPath $ pth <> "/" <> hostTarget <> "/debug"

addRustPaths :: LocalBuildInfo -> IO ()
addRustPaths lbi = do
  withRustPath confFlags bdir $ \rustPath -> do
   addToPath "LD_LIBRARY_PATH" rustPath
   addToPath "DYLD_LIBRARY_PATH" rustPath
  where
    addToPath :: String -> String -> IO ()
    addToPath pathVar path = do
      v <- try $ getEnv pathVar :: IO (Either SomeException String)
      setEnv pathVar (path ++ either (const "") (":" ++) v)

    confFlags = configFlags lbi
    bdir      = BuildDir $ buildDir lbi

rustArtifactsPath :: BuildDir -> IO FilePath
rustArtifactsPath (BuildDir relativeBuildDir) = do
  absoluteBuildDir <- canonicalizePath relativeBuildDir
  pure $ absoluteBuildDir <> "/rust"

buildRustWrapper :: ConfigFlags -> BuildDir -> IO ()
buildRustWrapper confFlags bdir = withConfiguredProgram confFlags cargoProgram $ \cargo -> do
  let verbosity = fromFlag $ configVerbosity confFlags
  pth <- rustArtifactsPath bdir
  void $ getProgramOutput verbose cargo
    ["+nightly"
    , "-v"
    , "-Z"
    , "unstable-options"
    , "-C"
    , "tiktoken-rs-hs-wrapper"
    , "cbuild"
    , "--target-dir"
    , pth
    ] `onException` (putStrLn "Make sure to install the rust nightly (\"rustup install nightly\") and the c-cargo applet (\"cargo install cargo-c\").")

extendConfigFlags :: LocalBuildInfo -> IO ConfigFlags
extendConfigFlags lbi = do
  withRustPath confFlags bdir $ \rustPath -> do
    pure $ confFlags {
           configExtraLibDirs     = rustPath : (configExtraLibDirs confFlags)
         , configExtraIncludeDirs = rustPath : (configExtraIncludeDirs confFlags)
         }
  where
    bdir      = BuildDir $ buildDir lbi
    confFlags = configFlags lbi

main :: IO ()
main = do
  let origUserHooks = simpleUserHooks

  defaultMainWithHooks origUserHooks {
      hookedPrograms = [ cargoProgram, rustcProgram ]

    -- Build the Rust wrapper library.
    --, preConf = \args confFlags -> do
    --    buildRustWrapper confFlags (BuildDir $ fromFlag (configDistPref confFlags) <> "/build")
    --    preConf origUserHooks args confFlags

    -- Ensures the built library path is available for the configure script
    -- to pick it up, for foreign dependency resolution.
    , confHook = \(genPkgDescription, hookedBuildInfo) confFlags -> do
        lb0 <- confHook origUserHooks (genPkgDescription, hookedBuildInfo) confFlags
        confFlags' <- extendConfigFlags lb0
        buildRustWrapper confFlags' (BuildDir $ buildDir lb0)
        confHook origUserHooks (genPkgDescription, hookedBuildInfo) confFlags'

    , buildHook = \packageDesc localBuildInfo userHooks buildFlags -> do
        buildRustWrapper (configFlags localBuildInfo) (BuildDir $ buildDir localBuildInfo)
        buildHook origUserHooks packageDesc localBuildInfo userHooks buildFlags

    -- Ensures that the built wrapper library is available both at
    -- LD_LIBRARY_PATH and DYLD_LIBRARY_PATH. Re-builds the Rust library.
    , testHook = \args packageDesc localBuildInfo userHooks testFlags -> do
        buildRustWrapper (configFlags localBuildInfo) (BuildDir $ buildDir localBuildInfo)
        addRustPaths localBuildInfo
        testHook origUserHooks args packageDesc localBuildInfo userHooks testFlags
   }
