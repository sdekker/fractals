{-# LANGUAGE MagicHash #-}

-- |
-- Module      : PropertyTests
-- Copyright   : (c) 2015 Stephen Dekker <steve.dekk@gmail.com>
-- License     : BSD3
--
-- Maintainer  : steve.dekk@gmail.com
-- Stability   : experimental
-- Portability : non-portable (MagicHash)

module PropertyTests (
  runTests,                    -- :: IO ()
  prop_grayCodeInversion,      -- :: Property
  prop_hilbertInversion,       -- :: Property
  prop_hilbertLocality,        -- :: Property
  prop_rotRBitCountInvariance, -- :: Property
  prop_rotLBitCountInvariance, -- :: Property
  prop_rotRInteger,            -- :: Property
  prop_rotLInteger,            -- :: Property
  prop_rotLRInversion          -- :: Property
  ) where

import Data.SpaceFillingCurve.Hilbert.Integer.Internal

import           GHC.Exts                (Int(I#))
import           GHC.Integer.Logarithms  (integerLog2#)

import           Data.Bits       (clearBit, popCount, setBit, shiftL, shiftR,
                                  testBit, (.&.), (.|.))
import           Test.QuickCheck (Gen, Property, arbitrary, choose, forAll,
                                  listOf1, quickCheck, suchThat)

------------------------
-- Test value generators

positiveInts :: Gen Int
positiveInts = (arbitrary :: Gen Int) `suchThat` (> 0)

nonNegativeInts :: Gen Int
nonNegativeInts = (arbitrary :: Gen Int) `suchThat` (>= 0)

nonNegativeIntegers :: Gen Integer
nonNegativeIntegers = (arbitrary :: Gen Integer) `suchThat` (>= 0)

nonNegativeVectors :: Gen [Integer]
nonNegativeVectors = listOf1 ((arbitrary :: Gen Integer) `suchThat` (>= 0))


--------------------------------------
-- Some property tests for this module

-- | Runs the QuickCheck property tests for the Hilbert encoder/decoder as
-- well as the internal helper functions.

runTests :: IO ()
runTests = do
    quickCheck prop_rotLBitCountInvariance
    quickCheck prop_rotRBitCountInvariance
    quickCheck prop_grayCodeInversion
    quickCheck prop_hilbertInversion
    quickCheck prop_hilbertLocality
    quickCheck prop_rotLRInversion
    quickCheck prop_rotLInteger
    quickCheck prop_rotRInteger

-- | The Gray encoding is invertible.

prop_grayCodeInversion :: Property
prop_grayCodeInversion = forAll nonNegativeInts $ \x ->
                         grayCodeInverse (grayCode x) == x

-- | The Hilbert curve encoding is invertible.

prop_hilbertInversion :: Property
prop_hilbertInversion = forAll nonNegativeVectors check
  where check ps = hilbertInverse m' n (hilbert m' ps :: Integer) == ps
          where m  = unboundedBitSize (maximum ps)
                n  = length ps
                m' = m + n - (m `mod` n)

-- | Two points co-located on the Hilbert curve should be within a unit
-- step in Cartesian space.

prop_hilbertLocality :: Property
prop_hilbertLocality = forAll nonNegativeVectors check
  where check ps = norm (diff ps ps') `near` 1
          where h   = hilbert m' ps :: Integer
                ps' = hilbertInverse m' n (h+1)
                n   = length ps
                m   = unboundedBitSize (maximum ps)
                m'  = m + n - (m `mod` n)

-- | The total population count of set bits must not change after a right
-- windowed rotate.

prop_rotRBitCountInvariance :: Property
prop_rotRBitCountInvariance = forAll positiveInts $ \width ->
                              forAll nonNegativeIntegers $ \x ->
                              forAll nonNegativeInts $ \i -> check width x i
  where check width x i = popCount (mask width .&. x) == popCount (rotR width x i)

-- | The total population count of set bits must not change after a left
-- windowed rotate.

prop_rotLBitCountInvariance :: Property
prop_rotLBitCountInvariance = forAll positiveInts $ \width ->
                              forAll nonNegativeIntegers $ \x ->
                              forAll nonNegativeInts $ \i -> check width x i
  where check width x i = popCount (mask width .&. x) == popCount (rotL width x i)

-- | Rotating a value right by one is equivalent to halving the value and
-- setting the last bit to 1 if the LSB in the initial value was set.

prop_rotRInteger :: Property
prop_rotRInteger = forAll positiveInts $ \width ->
                   forAll nonNegativeIntegers $ \x ->
                   forAll positiveInts $ \i -> check width x i
  where check width x i = rotR width x i == value
          where y = rotR width x (i-1)
                lsBit = testBit y 0
                value | lsBit     = (y `shiftR` 1) `setBit` (width - 1)
                      | otherwise = y `shiftR` 1

-- | Rotating a value left by one is equivalent to doubling the value after
-- clearing the MSB and setting the first bit to 1 if the MSB in the
-- initial value was set.

prop_rotLInteger :: Property
prop_rotLInteger = forAll nonNegativeIntegers $ \x ->
                   forAll (choose (1, unboundedBitSize x)) $ \width ->
                   forAll nonNegativeInts $ \i -> check width x i
  where check width x i = rotL width x i == value
          where y = rotL width x (i-1)
                msBit = y `testBit` (width - 1) 
                value | msBit     = ((y `clearBit` (width-1)) `shiftL` 1) .|. 1
                      | otherwise = y `shiftL` 1

-- | Check that the windowed rotate functions are the inverse of each
-- other, taking into account the fact that both functions truncate the
-- input value to the window width.

prop_rotLRInversion :: Property
prop_rotLRInversion = forAll positiveInts $ \width ->
                      forAll nonNegativeIntegers $ \x ->
                      forAll positiveInts $ \i -> check width x i
  where check width x i = rotR width (rotL width x i) i == mask width .&. x


-------------------------------------------
-- Utility functions for the property tests

-- | Calculates the number of bits required to represent an unbounded,
-- positive Integer.

unboundedBitSize :: Integer -> Int
unboundedBitSize i | i == 0    = 1
                   | otherwise = I# (integerLog2# i) + 1

-- | Calculates the norm of a given vector, returning the result as
-- a floating point number.

norm :: (Integral a, Floating b) => [a] -> b
norm a = sqrt (sum (map (fromIntegral . (^(2 :: Int))) a))

-- | Calculates the difference between two vectors.

diff :: Num a => [a] -> [a] -> [a]
diff = zipWith (-)

-- | Determines whether or not two 'Double' precision numbers are near
-- enough to be considered equal (that is, the displacement between them is
-- less than the machine epsilon).

near :: Double -> Double -> Bool
near a b = abs (a - b) < epsilon
  where epsilon = 2**(-53) :: Double

