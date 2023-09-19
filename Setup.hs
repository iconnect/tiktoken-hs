import Control.Exception (SomeException, try, onException)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
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

newtype RustTargetHost = RustTargetHost { getTargetHost :: String }
  deriving Show

getRustHostTarget :: ConfiguredProgram -> IO RustTargetHost
getRustHostTarget configuredProgram =
  let mkTarget = RustTargetHost . head . drop 1 . words . head . drop 4 . lines
  in mkTarget <$> getProgramOutput silent configuredProgram ["-vV"]

cargoProgram :: Program
cargoProgram = simpleProgram "cargo"

rustcProgram :: Program
rustcProgram = simpleProgram "rustc"

addToLdLibraryPath :: String -> IO ()
addToLdLibraryPath path = do
  let (ldLibraryPathVar, ldLibraryPathSep) =
        case buildOS of
          OSX -> ("DYLD_LIBRARY_PATH",":")
          _ -> ("LD_LIBRARY_PATH",":")
  v <- try $ getEnv ldLibraryPathVar :: IO (Either SomeException String)
  setEnv ldLibraryPathVar (path ++ either (const "") (ldLibraryPathSep ++) v)

-- | Adds the uninstalled.pc we build to the pkg_config_path.
addToPkgConfigPath :: String -> IO ()
addToPkgConfigPath path = do
  let pkgConfigPathVar = "PKG_CONFIG_PATH"
  v <- try $ getEnv pkgConfigPathVar :: IO (Either SomeException String)
  setEnv pkgConfigPathVar (path ++ either (const "") (":" ++) v)

withConfiguredProgram :: ConfigFlags -> Program -> (ConfiguredProgram -> IO a) -> IO a
withConfiguredProgram confFlags pgm act = do
  let verbosity = fromFlag $ configVerbosity confFlags
  (foundPgm, _) <- requireProgram verbosity pgm (configPrograms confFlags)
  act foundPgm

withRustc :: ConfigFlags -> (ConfiguredProgram -> IO a) -> IO a
withRustc confFlags = withConfiguredProgram confFlags rustcProgram

withCargo :: ConfigFlags -> (ConfiguredProgram -> IO a) -> IO a
withCargo confFlags = withConfiguredProgram confFlags cargoProgram

addRustPath :: ConfigFlags -> (String -> IO ()) -> IO ()
addRustPath confFlags pathAdder = do
  withRustc confFlags $ \rustc -> do
    (RustTargetHost hostTarget) <- getRustHostTarget rustc
    pathAdder $ rustArtifactsPath <> "/" <> hostTarget <> "/debug"

addRustWrapperToLdLibraryPath :: ConfigFlags -> IO ()
addRustWrapperToLdLibraryPath confFlags = addRustPath confFlags addToLdLibraryPath

addRustWrapperToPkgConfigPath :: ConfigFlags -> IO ()
addRustWrapperToPkgConfigPath confFlags = addRustPath confFlags addToPkgConfigPath

addRustPaths :: ConfigFlags -> IO ()
addRustPaths confFlags = do
  addRustWrapperToPkgConfigPath confFlags
  addRustWrapperToLdLibraryPath confFlags

rustArtifactsPath :: FilePath
rustArtifactsPath = "dist-newstyle/rust"

buildRustWrapper :: Verbosity -> ConfiguredProgram -> IO ()
buildRustWrapper verbosity cargo = void $
  getProgramOutput verbosity cargo
    ["+nightly"
    , "-Z"
    , "unstable-options"
    , "-C"
    , "tiktoken-rs-hs-wrapper"
    , "cbuild"
    , "--target-dir"
    , "../" <> rustArtifactsPath
    ]
  `onException` (putStrLn "Make sure to install the rust nightly (\"rustup install nightly\") and the c-cargo applet (\"cargo install cargo-c\").")

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let origUserHooks = simpleUserHooks

  defaultMainWithHooks origUserHooks {
      hookedPrograms = [ cargoProgram, rustcProgram ]

    --, preConf = \args confFlags -> do
    --    let verbosity = fromFlag $ configVerbosity confFlags
    --    withCargo confFlags (buildRustWrapper verbosity)
    --    addRustPaths confFlags
    --    preConf origUserHooks args confFlags

    , buildHook = \packageDesc localBuildInfo userHooks buildFlags -> do
        let confFlags  = configFlags localBuildInfo
        let verbosity  = fromFlag $ configVerbosity confFlags
        withCargo confFlags (buildRustWrapper verbosity)
        addRustPaths confFlags
        buildHook origUserHooks packageDesc localBuildInfo userHooks buildFlags

    , testHook = \args packageDesc localBuildInfo userHooks testFlags -> do
        let confFlags  = configFlags localBuildInfo
        let verbosity  = fromFlag $ configVerbosity confFlags
        withCargo confFlags (buildRustWrapper verbosity)
        addRustPaths confFlags
        testHook origUserHooks args packageDesc localBuildInfo userHooks testFlags
   }
