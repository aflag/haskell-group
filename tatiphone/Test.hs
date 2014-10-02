import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import TatiPhone
import Data.Char

main :: IO ()
main = hspec $ do
    describe "TatiPhone.translate" $ do
        it "should return xubinumbers" $ do
            translate "xubiru" `shouldBe` "277627"

        it "should have only xubinumbers" $
            property (\x -> all isDigit $ translate x)

        it "should always return the same length string" $
            property (\x -> (length $ translate x) == length x)
