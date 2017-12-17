{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitPrelude   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Poker
  ( Pip (..)
  , Suit (..)
  , HandF (..)
  , Hand
  , Outcome (..)
  , parseHand
  , unsafeHand
  , rank
  ) where

import           Control.Arrow                    ((&&&))
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8            (ByteString)
import           Data.Foldable                    (toList)
import           Data.Function                    (on)
import           Data.List                        (group, sort, sortBy)
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Monoid

data Pip
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
    deriving (Eq, Ord, Enum)

-- An exercise for the reader: replumb this so it all works aces low.
-- newtype AcesLow Pip = AL Pip deriving Eq
-- instance Ord AcesLow where
--   compare (AL Ace) _    = LT
--   compare _ (AL Ace)    = GT
--   compare (AL a) (AL b) = compare a b

instance Show Pip where
    show Two   = "2"
    show Three = "3"
    show Four  = "4"
    show Five  = "5"
    show Six   = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine  = "9"
    show Ten   = "T"
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show Ace   = "A"

parsePip :: Parser Pip
parsePip =
  anyChar >>= \case
    '2' -> pure Two
    '3' -> pure Three
    '4' -> pure Four
    '5' -> pure Five
    '6' -> pure Six
    '7' -> pure Seven
    '8' -> pure Eight
    '9' -> pure Nine
    'T' -> pure Ten
    'J' -> pure Jack
    'Q' -> pure Queen
    'K' -> pure King
    'A' -> pure Ace
    c   -> fail ("Unknown pip character " ++ [c])

data Suit = Hearts
          | Clubs
          | Spades
          | Diamonds deriving Eq

instance Show Suit where
    show Hearts   = "H"
    show Clubs    = "C"
    show Spades   = "S"
    show Diamonds = "D"

parseSuit :: Parser Suit
parseSuit = do
  c <- anyChar
  case c of
    'H' -> pure Hearts
    'C' -> pure Clubs
    'S' -> pure Spades
    'D' -> pure Diamonds
    _   -> fail ("Unknown suit character " ++ [c])

data Card = Card { pip :: Pip, suit :: Suit } deriving Eq

parseCard :: Parser Card
parseCard = Card <$> parsePip <*> parseSuit <* skipSpace

instance Ord Card where compare = compare `on` pip

instance Show Card where show Card{..} = show pip <> show suit

data HandF a = Hand a a a a a
  deriving (Eq, Functor, Foldable)

instance Show a => Show (HandF a) where show = unwords . toList . fmap show

first :: HandF a -> a
first (Hand a _ _ _ _) = a

type Hand = HandF Card

parseHand :: Parser Hand
parseHand = Hand <$> parseCard <*> parseCard <*> parseCard <*> parseCard <*> parseCard

unsafeHand :: ByteString -> Hand
unsafeHand b = case parseOnly parseHand b of
  Left e  -> error e
  Right h -> h

type Histogram = Map Pip Int

histo :: Hand -> Histogram
histo =
  M.fromList       -- map from pips to occurrences
  . fmap associate -- associate pips and occurrences
  . group          -- group contiguous pips into lists
  . sort           -- ensure pips are sorted
  . fmap pip       -- suits are irrelevant
  . toList where
    associate :: [Pip] -> (Pip, Int)
    associate = head &&& length

sorted :: (Foldable f, Ord a) => f a -> [a]
sorted = sort . toList

process :: Hand -> [(Pip, Int)]
process = sortBy higherFirst . M.toList . histo
  where higherFirst = compare `on` snd

comparator :: Hand -> Outcome
comparator h = case process h of
  [(f, 1), (o, 4)]         -> FourOfAKind o f
  [(t, 2), (x, 3)]         -> FullHouse x t
  [(t, 1), (a, 1), (b, 3)] -> ThreeOfAKind b a t
  [(a, 1), (b, 2), (c, 2)] -> TwoPair c' b' a where [b', c'] = sort [b, c]
  [a, b, c, (d, 2)]        -> OnePair d (sortBy (flip compare) (fmap fst [a,b,c]))
  other                    -> if length other /= 5
                              then error ("invariant violated: hand length /= 5 " <> show h)
                              else High (sortBy (flip compare) (fmap fst other))

data Outcome
    = High [Pip]
    | OnePair Pip [Pip]
    | TwoPair Pip Pip Pip
    | ThreeOfAKind Pip Pip Pip
    | Straight Pip
    | FullHouse Pip Pip
    | Flush Pip
    | FourOfAKind Pip Pip
    | StraightFlush Pip
    | RoyalFlush
      deriving (Show, Eq, Ord)

rank :: Hand -> Outcome
rank h
  | isRoyalFlush               = RoyalFlush
  | isStraightFlush            = StraightFlush highest
  | isFourOfAKind result       = result
  | isFlush                    = Flush highest
  | isFullHouse result         = result
  | isStraight                 = Straight highest
  | otherwise                  = result
    where result = comparator h
          highest = pip (maximum h)
          isRoyalFlush = isStraightFlush && highest == Ace
          isStraightFlush = isStraight && isFlush
          isFlush = all (== first suits) suits
          isStraight = sorted pips == [minimum pips .. maximum pips]
          suits = fmap suit h
          pips = fmap pip h

isFourOfAKind :: Outcome -> Bool
isFourOfAKind FourOfAKind{} = True
isFourOfAKind _             = False

isFullHouse :: Outcome -> Bool
isFullHouse FullHouse{} = True
isFullHouse _           = False

instance Ord (HandF Card) where compare = compare `on` rank
