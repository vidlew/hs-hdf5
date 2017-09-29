import Test.Hspec
import Spec.File (describeFile)
-- import Spec.Group (describeGroup)

main :: IO ()
main = hspec $ do
  describe "File interface" describeFile
--  describe "Group interface" describeGroup
