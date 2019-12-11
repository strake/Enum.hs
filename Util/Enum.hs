module Util.Enum (toEnumMay, fromEnum, predMay, succMay, compareEnum) where

import Prelude hiding (filter, fromEnum, head, tail)
import qualified Prelude

import Control.Monad
import Control.Exception
import Data.List (genericLength)
import System.IO.Unsafe

toEnumMay :: Enum a => Integer -> Maybe a
toEnumMay n
  | n < minInt, Just lower <- lowerEdgeMay, Just lower' <- predMay lower = [lower, lower'..] !!? (minInt - n)
  | n > maxInt, Just upper <- upperEdgeMay = [upper..] !!? (n - maxInt)
  | otherwise = (opMay toEnum <=< toIntMay) n

toIntMay :: Integer -> Maybe Int
toIntMay n = n' <$ guard (n == toInteger n') where n' = fromIntegral n

fromEnum :: Enum a => a -> Integer
fromEnum a
  | Just edge <- lowerEdgeMay, l <- genericLength [a..edge] - 1, l >= 0 = minInt - l
  | Just edge <- upperEdgeMay, l <- genericLength [edge..a] - 1, l >= 0 = maxInt + l
  | otherwise = (toInteger . Prelude.fromEnum) a

minInt, maxInt :: Integer
minInt = toEnum minBound
maxInt = toEnum maxBound

lowerEdgeMay, upperEdgeMay :: Enum a => Maybe a
lowerEdgeMay = opMay Prelude.toEnum minBound
upperEdgeMay = opMay Prelude.toEnum maxBound

compareEnum :: Enum a => a -> a -> Ordering
compareEnum a b
  | _:_ <- enumFromTo a b = LT
  | _:_ <- enumFromTo b a = GT
  | otherwise = EQ

predMay, succMay :: Enum a => a -> Maybe a
predMay = opMay pred
succMay = opMay succ

opMay :: (a -> b) -> a -> Maybe b
opMay f a = unsafePerformIO $ (Just <$> evaluate (f a)) `catches` handlers Nothing

handlers :: a -> [Handler a]
handlers a = [Handler $ \ (_ :: ArithException) -> pure a,
              Handler $ \ (_ :: ArrayException) -> pure a,
              Handler $ \ (_ :: AssertionFailed) -> pure a,
              Handler $ \ (_ :: NonTermination) -> pure a,
              Handler $ \ (_ :: NoMethodError) -> pure a,
              Handler $ \ (_ :: PatternMatchFail) -> pure a,
              Handler $ \ (_ :: RecConError) -> pure a,
              Handler $ \ (_ :: RecSelError) -> pure a,
              Handler $ \ (_ :: RecUpdError) -> pure a,
              Handler $ \ (_ :: ErrorCall) -> pure a]

(!!?) :: [a] -> Integer -> Maybe a
[] !!? _ = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? n = xs !!? (n-1)
