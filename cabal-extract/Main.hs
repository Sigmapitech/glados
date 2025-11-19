import Distribution.PackageDescription
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Verbosity (normal)

main :: IO ()
main = do
  gpd <- readGenericPackageDescription normal "ast/ast.cabal"
  print (allComponentNames gpd)

allComponentNames :: GenericPackageDescription -> [UnqualComponentName]
allComponentNames gpd =
  maybeToList (fmap (const (mkUnqualComponentName "lib")) (condLibrary gpd))
    ++ map fst (condSubLibraries gpd)
    ++ map fst (condExecutables gpd)
    ++ map fst (condTestSuites gpd)
    ++ map fst (condBenchmarks gpd)

maybeToList :: Maybe a -> [a]
maybeToList = maybe [] pure
