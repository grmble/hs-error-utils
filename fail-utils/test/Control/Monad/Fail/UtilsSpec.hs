module Control.Monad.Fail.UtilsSpec where

import Test.Hspec
import UriExample

spec :: Spec
spec = do
  describe "UriExample" $ do
    it "can parse a postgres uri" $
      let uri = "postgresql://satan@micros0ft:666/evil"
          expected = ("satan", "", "micros0ft", 666, "evil")
       in parsePostgresURI uri `shouldBe` Just expected

main :: IO ()
main = hspec spec
