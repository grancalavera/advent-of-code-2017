module AdventSpec
  ( spec
  )
where
import           Advent.Day1
import           Test.Hspec

spec :: Spec
spec = do
  describe "Day1" $ do
    it "sum of all the digits that match the next digit in a circular list" $ do
      captcha 1 [1, 1, 2, 2] `shouldBe` 3
      captcha 1 [1, 1, 1, 1] `shouldBe` 4
      captcha 1 [1, 2, 3, 4] `shouldBe` 0
      captcha 1 [9, 1, 2, 1, 2, 1, 2, 9] `shouldBe` 9
    it "sum of the digit halfway around the circular list" $ do
      captcha 2 [1, 2, 1, 2] `shouldBe` 6
      captcha 2 [1, 2, 2, 1] `shouldBe` 0
      captcha 3 [1, 2, 3, 4, 2, 5] `shouldBe` 4
      captcha 3 [1, 2, 3, 1, 2, 3] `shouldBe` 12
      captcha 4 [1, 2, 1, 3, 1, 4, 1, 5] `shouldBe` 4
