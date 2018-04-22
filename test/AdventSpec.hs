module AdventSpec
  ( spec
  )
where
import           Advent.Day1
import           Advent.Day2
import           Advent.Day4
import           Advent.Day5
import           Test.Hspec

spec :: Spec
spec = do

  describe "Day1: Captcha" $ do
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
    it "finds the unique pair of evenly divisible numbers in a row" $ do
      divsumRow [5, 9, 2, 8] `shouldBe` 4
      divsumRow [9, 4, 7, 3] `shouldBe` 3
      divsumRow [3, 8, 6, 5] `shouldBe` 2
    it "finds the divsum of the spreadsheet" $ do
      divsum [] `shouldBe` 0
      divsum [[5, 9, 2, 8], [9, 4, 7, 3], [3, 8, 6, 5]] `shouldBe` 9

  describe "Day 4: Day 4: High-Entropy Passphrases" $ do
    it "validates passphrases do not contain repeated words" $ do
      validatePassPhrase [] `shouldBe` False
      validatePassPhrase ["aa", "bb", "cc", "dd", "ee"] `shouldBe` True
      validatePassPhrase ["aa", "bb", "cc", "dd", "aa"] `shouldBe` False
      validatePassPhrase ["aa", "bb", "cc", "dd", "aaa"] `shouldBe` True
    it "validates no two words are anagrams" $ do
      validateNoAnagrams ["abcde", "fghij"] `shouldBe` True
      validateNoAnagrams ["abcde", "xyz", "ecdab"] `shouldBe` False
      validateNoAnagrams ["a", "ab", "abc", "abd", "abf", "abj"] `shouldBe` True
      validateNoAnagrams ["iiii", "oiii", "ooii", "oooi", "oooo"]
        `shouldBe` True
      validateNoAnagrams ["oiii", "ioii", "iioi", "iiio"] `shouldBe` False

  describe "Day 5: A Maze of Twisty Trampolines, All Alike" $ do
    it "finds the way out of the maze" $ do
      escapeMaze [] `shouldBe` 0
      escapeMaze [0, 3, 0, 1, -3] `shouldBe` 5
      escapeMaze [3,0,0,-2] `shouldBe` 8
    it "finds the way out of the maze with special rules" $ do
      escapeMazeSpecial [] `shouldBe` 0
      escapeMazeSpecial [0, 3, 0, 1, -3] `shouldBe` 10
