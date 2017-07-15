module SuperMemo.SuperMemoSpec where

import           Control.Monad
import           SuperMemo       (Days, Easiness, Interval (..), Quality (..),
                                  Repetitions, defaultInterval, minEasiness,
                                  nextInterval)
import           Test.Hspec
import           Test.QuickCheck

arbitraryNormalReps :: Gen Repetitions
arbitraryNormalReps = (suchThat arbitrary (> 2))

arbitraryEasiness :: Gen Easiness
arbitraryEasiness = (suchThat arbitrary (> 1.3))

arbitraryDays :: Gen Days
arbitraryDays  = (suchThat arbitrary (>= 1))

instance Arbitrary Interval where
  arbitrary = liftM3 Interval arbitraryNormalReps arbitraryEasiness arbitraryDays

instance Arbitrary Quality where
  arbitrary = arbitraryBoundedEnum

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Interval" $ do

    it "Days should increase for remembered responses" $
      forAll (elements [Normal, Perfect]) $
        \q i -> (days (nextInterval i q) > days i)

    it "Easiness should increase for remembered responses" $ property $
        \i -> (easiness (nextInterval i Perfect) > easiness i)

    it "Days should decrease or remain 1 for forgotten responses" $
      forAll (elements [Blackout, Forgot, Fuzzy]) $
        \q i -> (days (nextInterval i q) <= days i)

    it "Easiness should decrease for forgotten responses" $
      forAll (elements [Blackout, Forgot, Fuzzy]) $
        \q i -> (easiness (nextInterval i q) < easiness i)

    it "Easiness should always be more than 1.3" $ property $
        \i q -> (easiness (nextInterval i q) >= 1.3)

    it "First Review" $ do
      let Interval {days = d, easiness = e} = nextInterval defaultInterval Fuzzy
      d `shouldBe` 2
      e `shouldBe` 2.5

    it "Second Review" $ do
      let secondInterval = Interval 2 2.5 1
      let Interval {days = d, easiness = e} = nextInterval secondInterval Fuzzy
      d `shouldBe` 6
      e `shouldBe` 2.5

    it "Normal Review" $ do
      let normalInterval = Interval 6 2.5 3
      let Interval {days = d, easiness = e, reps = r} = nextInterval normalInterval Normal
      d `shouldBe` 8
      e `shouldBe` 2.5
      r `shouldBe` 7

    it "Perfect Review" $ do
      let normalInterval = Interval 6 2.5 3
      let Interval {days = d, easiness = e} = nextInterval normalInterval Perfect
      d `shouldBe` 8
      e `shouldBe` 2.6

    it "Hard Review" $ do
      let normalInterval = Interval 6 5 3
      let Interval {days = d, easiness = e} = nextInterval normalInterval Hard
      d `shouldBe` 15
      e `shouldBe` 4.86

    it "Bad Review" $ do
      let normalInterval = Interval 6 2.5 3
      let Interval {days = d, easiness = e, reps = r} = nextInterval normalInterval Blackout
      d `shouldBe` 1
      e `shouldBe` 1.7
      r `shouldBe` 1

    it "Low Easiness Review" $ do
      let normalInterval = Interval 6 1.3 3
      let Interval {days = d, easiness = e} = nextInterval normalInterval Forgot
      d `shouldBe` 1
      e `shouldBe` 1.3
