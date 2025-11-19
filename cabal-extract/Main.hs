import Distribution.PackageDescription
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Verbosity (normal)
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

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir = do
  entries <- listDirectory dir
  let paths = map (dir </>) entries
  concat <$> mapM classify paths
  where
    rejectedNamePattern :: String
    rejectedNamePattern = "^\\..*|^dist-newstyle$"
    classify :: FilePath -> IO [FilePath]
    classify path = do
      isDir <- doesDirectoryExist path
      isFile <- doesFileExist path
      case (takeFileName path, isDir, isFile) of
        (name, True, _) | name =~ rejectedNamePattern -> pure []
        (_, True, _) -> findCabalFiles path
        (_, _, True) | takeExtension path == ".cabal" -> pure [path]
        _ -> pure []

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  cabalFiles <- findCabalFiles cwd
  gpd <- mapM (readGenericPackageDescription normal) cabalFiles
  putStrLn "\nComponent names in each .cabal file:"
  mapM_ print (zip cabalFiles gpd)

-- let cabalFilePath = cwd </> "cabal-extract" </> "ast" </> "ast.cabal"
-- gpd <- readGenericPackageDescription normal cabalFilePath
-- print (allComponentNames gpd)
-- main = do
--   gpd <- readGenericPackageDescription normal "ast/ast.cabal"
--   print (allComponentNames gpd)

-- allComponentNames :: GenericPackageDescription -> [UnqualComponentName]
-- allComponentNames gpd =
--   maybeToList (fmap (const (mkUnqualComponentName "lib")) (condLibrary gpd))
--     ++ map fst (condSubLibraries gpd)
--     ++ map fst (condExecutables gpd)
--     ++ map fst (condTestSuites gpd)
--     ++ map fst (condBenchmarks gpd)

-- maybeToList :: Maybe a -> [a]
-- maybeToList = maybe [] pure
