module Main (main) where

import Control.Monad (forM_)
import Data.List (intercalate)
import qualified Data.Set as Set
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.Benchmark (Benchmark (..))
import Distribution.Types.BuildInfo (targetBuildDepends)
import Distribution.Types.Dependency (Dependency (..), depLibraries, depPkgName)
import Distribution.Types.Executable (Executable (..))
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.Library (Library (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.PackageId (pkgName, pkgVersion)
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.TestSuite (TestSuite (..))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Verbosity (deafening)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    listDirectory,
  )
import System.FilePath
  ( takeExtension,
    takeFileName,
    (</>),
  )
import Text.Pretty.Simple (pPrint)
import Text.Regex.TDFA ((=~))

data Component = Component
  { componentName :: String,
    componentDependencies :: [Dependency],
    componentReverseDependencies :: [String]
  }
  deriving (Show)

data Package = Package
  { packageName :: String,
    packageVersion :: String,
    packageExecutables :: [Component],
    packageLibraries :: [Component],
    packageTests :: [Component],
    packageBenchmarks :: [Component]
  }
  deriving (Show)

classify :: String -> String -> FilePath -> IO [FilePath]
classify rejectedNamePattern allowedNamePattern path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  case (takeFileName path ++ (if isFile then "." ++ takeExtension path else ""), isDir) of
    (name, _)
      | name =~ rejectedNamePattern -> pure []
      | name =~ allowedNamePattern -> pure [path]
    (_, True) -> findCabalFiles path
    _ -> pure []

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir =
  listDirectory dir >>= \entries ->
    let paths = map (dir </>) entries
     in concat <$> mapM (classify rejectedNamePattern allowedNamePattern) paths
  where
    rejectedNamePattern :: String
    rejectedNamePattern = "^\\..*|^dist-newstyle$"
    allowedNamePattern :: String
    allowedNamePattern = ".*\\.cabal"

parseCabalFile :: FilePath -> IO GenericPackageDescription
parseCabalFile = readGenericPackageDescription deafening

convertLibrary :: Library -> Component
convertLibrary lib =
  Component
    { componentName = case libName lib of
        LMainLibName ->
          "library"
        LSubLibName ucn ->
          unUnqualComponentName ucn,
      componentDependencies = targetBuildDepends (libBuildInfo lib),
      componentReverseDependencies = []
    }

convertExecutable :: Executable -> Component
convertExecutable exe =
  Component
    { componentName = unUnqualComponentName (exeName exe),
      componentDependencies = targetBuildDepends (buildInfo exe),
      componentReverseDependencies = []
    }

convertTest :: TestSuite -> Component
convertTest t =
  Component
    { componentName = unUnqualComponentName (testName t),
      componentDependencies = targetBuildDepends (testBuildInfo t),
      componentReverseDependencies = []
    }

convertBenchmark :: Benchmark -> Component
convertBenchmark b =
  Component
    { componentName = unUnqualComponentName (benchmarkName b),
      componentDependencies = targetBuildDepends (benchmarkBuildInfo b),
      componentReverseDependencies = []
    }

convertPackage :: PackageDescription -> Package
convertPackage pd =
  Package
    { packageName =
        unPackageName (pkgName (package pd)),
      packageVersion =
        prettyShow (pkgVersion (package pd)),
      packageLibraries =
        maybe [] (pure . convertLibrary) (library pd)
          ++ map convertLibrary (subLibraries pd),
      packageExecutables =
        map convertExecutable (executables pd),
      packageTests =
        map convertTest (testSuites pd),
      packageBenchmarks =
        map convertBenchmark (benchmarks pd)
    }

depToString :: Dependency -> String
depToString dep =
  intercalate "," $
    map fmt (NES.toList (depLibraries dep))
  where
    pkg = unPackageName (depPkgName dep)
    fmt LMainLibName = pkg
    fmt (LSubLibName subname) = pkg ++ ":" ++ unUnqualComponentName subname

allComponents :: Package -> [Component]
allComponents p =
  packageLibraries p
    ++ packageExecutables p
    ++ packageTests p
    ++ packageBenchmarks p

innerComponent :: [Package] -> [String]
innerComponent =
  concatMap
    ( \pkg ->
        map
          ( \comp -> case componentName comp of
              "library" -> packageName pkg
              name -> packageName pkg ++ ":" ++ name
          )
          (allComponents pkg)
    )

trimComponent :: Set.Set String -> Component -> Component
trimComponent allowed comp =
  comp
    { componentDependencies =
        filter
          (\dep -> depToString dep `Set.member` allowed)
          (componentDependencies comp)
    }

trim :: Package -> [String] -> Package
trim pkg comps =
  let allowed = Set.fromList comps
      trimC = trimComponent allowed
   in pkg
        { packageLibraries = map trimC (packageLibraries pkg),
          packageExecutables = map trimC (packageExecutables pkg),
          packageTests = map trimC (packageTests pkg),
          packageBenchmarks = map trimC (packageBenchmarks pkg)
        }

main :: IO ()
main =
  getCurrentDirectory >>= findCabalFiles >>= \cabalFiles -> do
    gpds <- mapM parseCabalFile cabalFiles
    let fpds = map flattenPackageDescription gpds
        pkgs = map convertPackage fpds
        trimmedPkgs = map (`trim` innerComponent pkgs) pkgs
    mapM_ pPrint trimmedPkgs
