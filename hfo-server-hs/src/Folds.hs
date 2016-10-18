{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Folds where

import Data.Monoid
import Control.Applicative (liftA2)
import Debug.Trace

import qualified Data.Foldable

-- | Beautiful Folds from Gabriel Gonzales Munihac Talk
--   https://github.com/Gabriel439/slides/blob/master/munihac/foldmap.md
--
data Fold i o = forall m . Monoid m => Fold (i -> m) (m -> o)

fold :: Fold i o -> [i] -> o
fold (Fold tally summarize) is = summarize (reduce (map tally is))
  where
    reduce = Data.Foldable.foldl' (<>) mempty

-- | Combine mechanics
--
data Pair a b = P !a !b

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty = P mempty mempty

    mappend (P aL bL) (P aR bR) = P (mappend aL aR) (mappend bL bR)

instance Functor (Fold i) where
    fmap k (Fold tally summarize) = Fold tally (k . summarize)

instance Applicative (Fold i) where
    pure o = Fold (\_ -> ()) (\_ -> o)

    Fold tallyF summarizeF <*> Fold tallyX summarizeX = Fold tally summarize
      where
        tally i = P (tallyF i) (tallyX i)

        summarize (P mF mX) = summarizeF mF (summarizeX mX)

-- | Num / Floating / Factional Instances
--

instance Num b => Num (Fold a b) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance Fractional b => Fractional (Fold a b) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance Floating b => Floating (Fold a b) where
    pi = pure pi

    exp   = fmap exp
    sqrt  = fmap sqrt
    log   = fmap log
    sin   = fmap sin
    tan   = fmap tan
    cos   = fmap cos
    asin  = fmap sin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    tanh  = fmap tanh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

    (**)    = liftA2 (**)
    logBase = liftA2 logBase

-- | Some imports from Data.Monoid

sumF :: (Num n, Show n) => Fold n n
sumF = Fold Sum getSum

lengthF :: (Num n, Show n) => Fold i n
lengthF = Fold (\_ -> Sum 1) getSum

-- | Numerical stable average implementation
--
data Average a = Average { samples :: !a, value :: !a }

instance Fractional a => Monoid (Average a) where
    mempty = Average 0 0

    mappend (Average nL xL) (Average nR xR) = Average n x
        where
            n = nL + nR

            x = (xL * nL + xR * nR) / (nL + nR)

averageF :: (Fractional a, Show a) => Fold a a
averageF = Fold tally summarize
    where
        tally x = Average 1 x

        summarize (Average _ x) = x

-- | Standard Deviation Monoid implementation without catastrophic cancellation
--   
--   ---------------------------------------
--   |TODO: Did not work with this method!!|
---  ---------------------------------------
--
--   Var(X) = E[(X-100)^2] - E[(X-100)]^2 
--   Std(X) = sqrt(Var(X))
-- 
--   Variance is invariant with respect to changes in a location parameter k
--   
--   Var(X-k) = Var(X)
--
--   While subtracting a constant factor we get a more stable version for
--   long floating point lists with similar elements, because sometimes
--   we try to take sqrt from a negative number
--
--   The number 100 is picked because it works (stochastically found)
--
--   Example:
--   > let list = [0.979753702707849,0.9797537115429252,0.9797536939389248
--                ,0.979753745910182,0.9797537191681568,0.9797537115429252
--                ,0.9797537115429252,0.9797537082683674,0.9797537099196036
--                ,0.9797537205359157,0.9797537088957801,0.9797537115429252]
--   > fold stdF list
--   NaN
--
--   Because:
--       E[X^2] = 0.9599173380583315
--       E[X]^2 = 0.9797537129597068
--       E[X^2] - E[X]^2 = -1.1102230246251565e-16
--       sqrt(-1.1102230246251565e-16) = NaN

stdF :: (Floating a, Show a) => Fold a a
stdF = sqrt ((sumOfSquares / lengthF) - (sumF' / lengthF)^2)
    where
        sumOfSquares = Fold (Sum . (^2) . (\x -> x - 100)) getSum

        sumF' = Fold (Sum . (\x -> x - 100)) getSum