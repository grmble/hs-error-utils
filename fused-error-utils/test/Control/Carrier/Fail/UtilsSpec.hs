module Control.Carrier.Fail.UtilsSpec where

import Control.Carrier.Fail.Utils
import Data.Function ((&))
import Test.Hspec

spec :: Spec
spec = do
  describe "runPure" $ do
    it "extracts a MonadFail error string as an Either" $
      (failNothing "boo!" Nothing & runPure)
        `shouldBe` (Left "boo!" :: Either String Int)

main :: IO ()
main = hspec spec
