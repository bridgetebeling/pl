-- src/TypeClasses.hs

module TypeClasses where

import Data.List (zipWith)
import Data.Monoid
import Data.Semigroup

-- Vec type with list of Doubles
newtype Vec = Vec [Double]

-- Custom Show instance to display Vec nicely
instance Show Vec where
  show (Vec xs) = "Vec " ++ show xs

-- Num instance: elementwise arithmetic
instance Num Vec where
  (Vec a) + (Vec b) = Vec $ zipWith (+) a b
  (Vec a) - (Vec b) = Vec $ zipWith (-) a b
  (Vec a) * (Vec b) = Vec $ zipWith (*) a b
  negate (Vec a) = Vec $ map negate a
  abs (Vec a) = Vec $ map abs a
  signum (Vec a) = Vec $ map signum a
  fromInteger x = Vec $ repeat (fromInteger x)

-- Equality check using elementwise comparison
instance Eq Vec where
  (Vec a) == (Vec b) = and $ zipWith (==) a b

-- Lexicographical comparison using standard list rules
instance Ord Vec where
  compare (Vec a) (Vec b) = compare a b

-- Custom typeclass for types that can have magnitude
class VecT a where
  magnitude :: a -> Double

-- Implementation for Vec: Euclidean norm
instance VecT Vec where
  magnitude (Vec xs) = sqrt $ sum $ map (^2) xs

-- Semigroup uses elementwise addition
instance Semigroup Vec where
  (<>) = (+)

-- Monoid uses zero-vector (infinite list of 0.0) as identity
instance Monoid Vec where
  mempty = Vec $ repeat 0.0
  mappend = (<>)
  mconcat = foldr (<>) mempty