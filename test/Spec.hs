import Test.Hspec
import Lib.AWS.Lambda.Events.SNSEvent

main :: IO ()
main = hspec $ do
  describe "upperCamelize" $ do
    it "should succeed" $ do
      upperCamelize "text" `shouldBe` "Text"
  describe "dropLastPrime" $ do
    it "should succeed" $ do
      dropLastPrime "type'" `shouldBe` "type"
  describe "dropLastPrime . upperCamelize" $ do
    it "should succeed" $ do
      (dropLastPrime . upperCamelize $ "type'") `shouldBe` "Type"

