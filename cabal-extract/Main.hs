{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.Benchmark (Benchmark (..))
import Distribution.Types.BuildInfo (targetBuildDepends)
import Distribution.Types.Component (Component (..))
import Distribution.Types.Dependency (Dependency (..), depLibraries, depPkgName)
import Distribution.Types.Executable (Executable (..))
import Distribution.Types.ForeignLib (foreignLibBuildInfo, foreignLibName)
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
import Text.Regex.TDFA ((=~))

data ComputedComponent = ComputedComponent
  { componentName :: String,
    componentDependencies :: [Dependency],
    componentReverseDependencies :: [String]
  }
  deriving (Show)

data Package = Package
  { packageName :: String,
    packageVersion :: String,
    packageExecutables :: [ComputedComponent],
    packageLibraries :: [ComputedComponent],
    packageTests :: [ComputedComponent],
    packageBenchmarks :: [ComputedComponent]
  }
  deriving (Show)

instance ToJSON ComputedComponent where
  toJSON comp =
    object
      [ "name" .= componentName comp,
        "dependencies" .= map depToString (componentDependencies comp),
        "reverseDependencies" .= componentReverseDependencies comp
      ]

instance ToJSON Package where
  toJSON pkg =
    object
      [ "name" .= packageName pkg,
        "version" .= packageVersion pkg,
        "libraries" .= packageLibraries pkg,
        "executables" .= packageExecutables pkg,
        "tests" .= packageTests pkg,
        "benchmarks" .= packageBenchmarks pkg
      ]

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

computeComponentDeps :: Component -> ComputedComponent
computeComponentDeps = dispatcher
  where
    retrieveLibName lib = case libName lib of
      LMainLibName -> "library"
      LSubLibName ucn -> ucn

    dispatcher :: Component -> ComputedComponent
    dispatcher (CLib lib) = wrap lib retrieveLibName libBuildInfo
    dispatcher (CExe exe) = wrap exe exeName buildInfo
    dispatcher (CTest ts) = wrap ts testName testBuildInfo
    dispatcher (CBench bk) = wrap bk benchmarkName benchmarkBuildInfo
    dispatcher (CFLib lib) = wrap lib foreignLibName foreignLibBuildInfo

    wrap blob nameRetriever dependencyRetriever =
      ComputedComponent
        { componentName = unUnqualComponentName (nameRetriever blob),
          componentDependencies = targetBuildDepends (dependencyRetriever blob),
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
        maybe [] (pure . convToCC . CLib) (library pd)
          ++ map (convToCC . CLib) (subLibraries pd),
      packageExecutables = map (convToCC . CExe) (executables pd),
      packageTests =
        map (convToCC . CTest) (testSuites pd),
      packageBenchmarks =
        map (convToCC . CBench) (benchmarks pd)
    }
  where
    convToCC = computeComponentDeps

depToString :: Dependency -> String
depToString dep =
  intercalate "," $
    map fmt (NES.toList (depLibraries dep))
  where
    pkg = unPackageName (depPkgName dep)
    fmt LMainLibName = pkg
    fmt (LSubLibName subname) = pkg ++ ":" ++ unUnqualComponentName subname

allComponents :: Package -> [ComputedComponent]
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

trimComponent :: Set.Set String -> ComputedComponent -> ComputedComponent
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

buildReverseDependencies :: [Package] -> [Package]
buildReverseDependencies pkgs =
  let compDepMap =
        foldr
          ( \pkg acc ->
              foldr
                ( \comp acc' ->
                    let compKey = case componentName comp of
                          "library" -> packageName pkg
                          name -> packageName pkg ++ ":" ++ name
                     in foldr
                          ( \dep acc'' ->
                              let depKey = depToString dep
                               in if depKey `elem` map fst acc''
                                    then
                                      map
                                        ( \entry ->
                                            if fst entry == depKey
                                              then (fst entry, snd entry ++ [compKey])
                                              else entry
                                        )
                                        acc''
                                    else (depKey, [compKey]) : acc''
                          )
                          acc'
                          (componentDependencies comp)
                )
                acc
                (allComponents pkg)
          )
          []
          pkgs
      compDepMapLookup key = fromMaybe [] (lookup key compDepMap)
      addReverseDeps pkg =
        let addRevDepToComp comp =
              comp
                { componentReverseDependencies =
                    compDepMapLookup $
                      case componentName comp of
                        "library" -> packageName pkg
                        name -> packageName pkg ++ ":" ++ name
                }
         in pkg
              { packageLibraries = map addRevDepToComp (packageLibraries pkg),
                packageExecutables = map addRevDepToComp (packageExecutables pkg),
                packageTests = map addRevDepToComp (packageTests pkg),
                packageBenchmarks = map addRevDepToComp (packageBenchmarks pkg)
              }
   in map addReverseDeps pkgs

main :: IO ()
main =
  getCurrentDirectory >>= findCabalFiles >>= \cabalFiles -> do
    gpds <- mapM parseCabalFile cabalFiles
    let fpds = map flattenPackageDescription gpds
        pkgs = map convertPackage fpds
        trimmedPkgs = map (`trim` innerComponent pkgs) pkgs
        finalPkgs = buildReverseDependencies trimmedPkgs
    BL.putStrLn (encode finalPkgs)
