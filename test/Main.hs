{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Poker

import           Control.Lens
import           Test.Hspec.Lens

makePrisms ''Pip
makePrisms ''Outcome

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "poker" $ do
  it "should pass test 1" $ do
    let h1 = unsafeHand "5H 5C 6S 7S KD"
    let h2 = unsafeHand "2C 3S 8S 8D TD"
    rank h1 `shouldHave` (_OnePair._1._Five)
    rank h2 `shouldHave` (_OnePair._1._Eight)
    compare h1 h2 `shouldBe` LT
  it "should pass test 2" $ do
    let h1 = unsafeHand "5D 8C 9S JS AC"
    let h2 = unsafeHand "2C 5C 7D 8S QH"
    rank h1 `shouldHave` (_High._head._Ace)
    rank h2 `shouldHave` (_High._head._Queen)
    compare h1 h2 `shouldBe` GT
  it "should pass test 3" $ do
    let h1 = unsafeHand "2D 9C AS AH AC"
    let h2 = unsafeHand "3D 6D 7D TD QD"
    rank h1 `shouldHave` (_ThreeOfAKind._1._Ace)
    rank h2 `shouldHave` _Flush
    compare h1 h2 `shouldBe` LT
  it "should pass test 4" $ do
    let h1 = unsafeHand "4D 6S 9H QH QC"
    let h2 = unsafeHand "3D 6D 7H QD QS"
    rank h1 `shouldHave` (_OnePair._2._head._Nine)
    rank h2 `shouldHave` (_OnePair._2._head._Seven)
    compare h1 h2 `shouldBe` GT
  it "should pass test 5" $ do
    let h1 = unsafeHand "2H 2D 4C 4D 4S"
    let h2 = unsafeHand "3C 3D 3S 9S 9D"
    rank h1 `shouldHave` (_FullHouse._1._Four)
    rank h2 `shouldHave` (_FullHouse._1._Three)
    compare h1 h2 `shouldBe` GT
  it "should pass my cunning test" $ do
    let h1 = unsafeHand "2H 2C TD JD KH"
    let h2 = unsafeHand "2D 2S 4D TD 9H"
    rank h1 `shouldHave` (_OnePair._1._Two)
    rank h2 `shouldHave` (_OnePair._1._Two)
    rank h1 `shouldHave` (_OnePair._2._head._King)
    compare h1 h2 `shouldBe` GT
  it "should pass a degenerate two-pair situation" $ do
    let h1 = unsafeHand "2H 2C 3S 3D KH"
    let h2 = unsafeHand "2S 2D 3H 3C 9H"
    rank h1 `shouldHave` (_TwoPair._1._Three)
    rank h1 `shouldHave` (_TwoPair._2._Two)
    rank h1 `shouldHave` (_TwoPair._3._King)
    rank h2 `shouldHave` (_TwoPair._1._Three)
    rank h2 `shouldHave` (_TwoPair._2._Two)
    rank h2 `shouldHave` (_TwoPair._3._Nine)
    compare h1 h2 `shouldBe` GT
  it "should not have spurious Highs" $ do
    let h1 = unsafeHand "9D KS 2D 3C KC"
    let h2 = unsafeHand "4S 8C KH 6C JC"
    compare h1 h2 `shouldBe` GT
