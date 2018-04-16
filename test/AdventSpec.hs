module AdventSpec
  ( spec
  )
where
import           Advent.Day1
import           Test.Hspec

spec :: Spec
spec = do
  describe "Day1" $ do
    it "sum of all the digits that match the next digit" $ do
      captcha [1, 1, 2, 2] `shouldBe` 3
      captcha [1, 1, 1, 1] `shouldBe` 4
      captcha [1, 2, 3, 4] `shouldBe` 0
      captcha [9, 1, 2, 1, 2, 1, 2, 9] `shouldBe` 9
