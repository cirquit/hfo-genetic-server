{-# LANGUAGE BangPatterns #-}

module CrossEntropy(
     DistributionSet(..)
   , NormalDist(..)
   , sampleFrom
   , createNormalDist
   , createDistributionSet
   , createDistributionSetFrom
   , sampleOnceFromDistSet
   , getMeanAndStd
   , normal
    ) where

import Control.Monad        (replicateM)
import Control.Monad.Random (getRandomR, MonadRandom())
import Data.List            (transpose)
import Debug.Trace          (trace)

import HFO.Agent.Data (Offense(..))
import Folds

data NormalDist = NormalDist { mean :: Double, std :: Double } 
    deriving Show

data DistributionSet = DistributionSet { dists :: [NormalDist] }
    deriving Show

-- | sample once from the normal distribution
--
sampleFrom :: MonadRandom m => NormalDist -> m Double
sampleFrom (NormalDist mean std) = normal mean std

-- | create a normal distribution with upper and lower bounds for mean and standard deviation
--
--                                   (-µ,µ)    (-σ,σ)
createNormalDist :: MonadRandom m => Double -> Double -> m NormalDist
createNormalDist meanBounds stdBounds = NormalDist <$> getRandomR (-meanBounds, meanBounds)
                                                   <*> getRandomR (-stdBounds, stdBounds)

-- | create a distribution set with n distributions with upper and lower bounds for mean and standard deviation
--
--                                        (-µ,µ)    (-σ,σ)     n
createDistributionSet :: MonadRandom m => Double -> Double -> Int -> m DistributionSet
createDistributionSet meanBounds stdBounds n = DistributionSet <$> replicateM n (createNormalDist meanBounds stdBounds)

-- | creates a distribution set from established mean and stds
--
--                               µ       σ
createDistributionSetFrom :: [(Double, Double)] -> DistributionSet
createDistributionSetFrom meanAndStds = DistributionSet $ map (uncurry NormalDist) meanAndStds

-- | sample once from every normal distribution in the distribution set
--
sampleOnceFromDistSet :: MonadRandom m => DistributionSet -> m [Double]
sampleOnceFromDistSet (DistributionSet dists) = mapM sampleFrom dists

-- | gets the mean and standart deviation for every coefficient in the encoding
--   in a somewhat efficient manner
--   
--   can be improved tho if one can figure out a way to traverse N lists simultaneosly
--
getMeanAndStd :: [Offense] -> [(Double, Double)]
getMeanAndStd offs =
    let encodings :: [[Double]]
        encodings = map offEncoding offs

        coefficientMatrix :: [[Double]]
        coefficientMatrix = transpose encodings

    in map meanAndStd coefficientMatrix


-- | Robust implementation of variance to (hopefully) negate catastrophic cancellation
--   Read more at Folds.stdF
--
sumVar :: Double -> [Double] -> Double
sumVar m = sum . map ((^2) . subtract m)

meanAndStd :: [Double] -> (Double, Double)
meanAndStd l
    | n > 1     = (mean, sqrt $ sumVar mean l / n)
    | otherwise = (mean, 0)
    where
      (mean, n) = fold ((,) <$> averageF <*> lengthF) l


-- | copied the implementation for normal distribution for MonadRandom support with the box-muller method from
--   http://stackoverflow.com/questions/13936502/sampling-a-normal-distribution-using-control-monad-monadrandom

-- | Generates uniform random variables.
--
unif :: (MonadRandom m) => m Double
unif = getRandomR (0,1)

-- | Generate two samples from the standard normal distribution, using
--   the Box-Muller method.
--
stdNormals :: (MonadRandom m) => m (Double,Double)
stdNormals = do
    u <- unif
    v <- unif
    let r    = sqrt((-2) * log u)
        arg1 = cos (2 * pi * v)
        arg2 = sin (2 * pi * v)
    return (r * arg1, r * arg2)

-- | Generate a single sample from the standard normal distribution, by
--   generating two samples and throwing away the second one.
--
stdNormal :: (MonadRandom m) => m Double
stdNormal = do
    (x,_) <- stdNormals
    return x

-- | Generate a sample from the standard normal distribution with a given
--   mean and standard deviation
--
normal :: (MonadRandom m) => Double -> Double -> m Double
normal mu sigma = do
    x <- stdNormal
    return $ mu + sigma * x
