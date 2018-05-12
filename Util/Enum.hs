module Util.Enum (toEnumMay, fromEnum, predMay, succMay) where

import Prelude hiding (filter, fromEnum, head, tail)
import qualified Prelude
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup (..), stimes)
import System.IO.Unsafe

toEnumMay :: Enum a => Integer -> Maybe a
toEnumMay n = opMay (toEnum . fromIntegral) n <|> toEnumSafe n

toEnumSafe :: Enum a => Integer -> Maybe a
toEnumSafe n = stimes (abs n) (EndoM $ bool succMay predMay $ n < 0) `endoM` toEnum 0

fromEnum :: Enum a => a -> Integer
fromEnum = fromMaybe <$> fromEnumSafe <*> opMay (fromIntegral . Prelude.fromEnum)

fromEnumSafe :: Enum a => a -> Integer
fromEnumSafe a = head $ f `mapMaybe` let go n = n :. negate n :. go (n+1) in 0 :. go 1
  where f n = n <$ (guard . (== 0) =<< opMay Prelude.fromEnum =<<
                    stimes (abs n) (EndoM $ bool succMay predMay $ n < 0) `endoM` a)

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

newtype EndoM m a = EndoM { endoM :: a -> m a }
instance Monad m => Semigroup (EndoM m a) where EndoM f <> EndoM g = EndoM (f >=> g)
instance Monad m => Monoid (EndoM m a) where
    mappend = (<>)
    mempty = EndoM pure

infixr 5 :.
data Stream a = (:.) { head :: a, tail :: Stream a }
  deriving (Functor, Foldable)

instance Applicative Stream where
    pure a = a :. pure a
    f :. fs <*> x :. xs = f x :. (fs <*> xs)

instance Monad Stream where
    xs >>= f = join (f <$> xs) where join (a :. as) = head a :. join (tail <$> as)

mapMaybe :: (a -> Maybe b) -> Stream a -> Stream b
mapMaybe f (a :. as) = maybe id (:.) (f a) $ mapMaybe f as
