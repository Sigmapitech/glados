import qualified SpecConfig
import qualified SpecFormatter
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  SpecFormatter.spec
  SpecConfig.spec
