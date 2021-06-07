import           Spec.File  (describeFile)
import           Spec.Group (describeGroup)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "File interface" describeFile
  describe "Group interface" describeGroup
