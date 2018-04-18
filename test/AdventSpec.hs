module AdventSpec
  ( spec
  )
where
import           Advent.Day1
import           Advent.Day2
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
  describe "Day 2: Corruption Checksum" $ do
    it "calculates the checksum of each row in the spreadsheet" $ do
      checksumRow [5, 1, 9, 5] `shouldBe` Just 8
      checksumRow [7, 5, 3] `shouldBe` Just 4
      checksumRow [2, 4, 6, 8] `shouldBe` Just 6
    it "calculates the checksum of the spreadsheet" $ do
      checksum [] `shouldBe` 0
      checksum [[5, 1, 9, 5], [7, 5, 3], [2, 4, 6, 8]] `shouldBe` 18
