{-# LANGUAGE TypeApplications #-}
module Main where

import Prelude hiding (fromEnum)

import Data.Foldable
import Data.Functor.Const (Const (..))
import Data.Int
import Data.Maybe (isNothing)
import Data.Word

import Test.SmallCheck
import Test.Tasty
import Test.Tasty.HUnit

import Enum

main :: IO ()
main = defaultMain $ testGroup "" $ asum
  [ getConst (boundsTests @Int)
  , getConst (boundsTests @Int8)
  , getConst (boundsTests @Word)
  , getConst (boundsTests @Word8)
  , getConst (boundsTests @Char)
  , getConst (boundsTests @Bool)
  , getConst (boundsTests @Ordering)
  , let lower = toInteger (minBound :: Int)
        upper = toInteger (maxBound :: Int)
        lower' = lower - 1
        upper' = upper + 1
    in
      [lower', lower, 0, upper, upper'] >>= \ n ->
      [ testCase "toEnumMay" $ assertBool "" $ toEnumMay n == Just n
      , testCase ("fromEnum " ++ show n) $ assertBool "" $ fromEnum n == n]
  ]

boundsTests :: ∀ a . (Bounded a, Enum a) => Const [TestTree] a
boundsTests = Const
  [ testCase "predMay minBound" $ assertBool "" . isNothing $ predMay (minBound :: a)
  , testCase "succMay maxBound" $ assertBool "" . isNothing $ succMay (maxBound :: a)
  ]
