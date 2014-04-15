{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

import System.Random.MWC
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad (filterM, liftM, replicateM)

class Generatable t where
  generate :: PrimMonad m => Gen (PrimState m) -> m t

data Role = Ay2 | Ay1 | JTP | Prof deriving (Enum, Show)
instance Generatable Role where
  generate gen = toEnum `liftM` uniformR (0, 3) gen

data Professor = Professor { availability :: [Bool],
                             roles :: [Role],
                             quality :: Course -> Integer
                           }
instance Show Professor where
  show p = let available = let pairs = zip [1..] (availability p)
                           in  show . map fst $ filter snd pairs
               r = show $ roles p
           in "{roles = " ++ r ++ ", available = " ++ available ++ "}"

instance Generatable Professor where
  generate gen = do
    availability <- replicateM 365 (uniform gen)
    i <- uniformR (-1, 3) gen
    let roles = map toEnum [0 .. i]
    let quality = const 8
    return $ Professor { availability = availability,
                         roles = roles,
                         quality = quality}

-- !! i gives the day of the ith class
type WeeklyPlan = [Integer]
instance Generatable WeeklyPlan where
  generate gen = filterM (const $ uniform gen) [0..6]

type ClassRequirement = [(Role, Integer)]
instance Generatable ClassRequirement where
  generate gen = do
    i <- uniformR (1, 3) gen
    let roles = map (toEnum . (3 -)) [0 .. i]
    counts :: [Int] <- replicateM i (uniformR (1, 3) gen)
    return . zip roles . map fromIntegral $ counts

data Course = Course { name :: String,
                       availablePatterns :: [Integer],
                       perClassRequirements :: [ClassRequirement],
                       numberOfClasses :: Integer
                     } deriving Show

instance Generatable Course where
  generate gen = do
    patterns <- filterM (const $ uniform gen) [0..10]
    numberOfClasses :: Int <-  uniformR (1, 20) gen
    reqs <- replicateM numberOfClasses $ generate gen
    return $ Course { name = "Foo", availablePatterns = patterns, perClassRequirements = reqs, numberOfClasses = fromIntegral numberOfClasses}


data Problem = Problem {
                         courses :: [Course],
                         professors :: [Professor],
                         plans :: [WeeklyPlan]
                       } deriving Show
instance Generatable Problem where
  generate gen = do
    courses <- replicateM 4 $ generate gen
    professors <- replicateM 20 $ generate gen
    plans <- replicateM 7 $ generate gen
    return $ Problem { courses = courses, professors = professors, plans = plans }
generateCourse :: GenIO -> IO [Int]
generateCourse = \gen -> do
  v <- uniform gen
  return [v]

main :: IO ()
main = return ()
