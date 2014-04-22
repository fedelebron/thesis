{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}

import System.Random.MWC
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad (filterM, liftM, replicateM)

type WeeklyPattern = [Integer]
data Context = Context { numberOfProfessors :: Int,
                         numberOfDays :: Int,
                         numberOfCourses :: Int,
                         numberOfPatterns :: Int,
                         numberOfRoles :: Int,
                         numberOfPlans :: Int,
                         maxNumberOfClasses :: Int
                       }

class Generatable t where
  generate :: PrimMonad m => Context -> Gen (PrimState m) -> m t

type Role = Int
instance Generatable Role where
  generate ctx = uniformR (1, numberOfRoles ctx)

data Professor = Professor { availability :: [Bool],
                             roles :: [Role],
                             quality :: Course -> Int
                           }
instance Show Professor where
  show p = let available = let pairs = zip [1..] (availability p)
                           in  show . map fst $ filter snd pairs
               r = show $ roles p
           in "{roles = " ++ r ++ ", available = " ++ available ++ "}"

instance Generatable Professor where
  generate ctx gen = do
    availability <- replicateM (numberOfDays ctx) (uniform gen)
    i <- uniformR (0, numberOfRoles ctx - 1) gen
    let roles = [0 .. i]
    let quality = const 8
    return $ Professor { availability = availability,
                         roles = roles,
                         quality = quality}

type WeeklyPlan = [Int]
instance Generatable WeeklyPlan where
  generate _ gen = filterM (const $ uniform gen) [0..6]

toYearlyDates :: WeeklyPlan -> [Int]
toYearlyDates = concat . zipWith map (map (+) [0,7..365]) . repeat

type ClassRequirement = [(Role, Int)]
instance Generatable ClassRequirement where
  generate ctx gen = do
    i <- uniformR (0, numberOfRoles ctx - 1) gen
    let roles = [0 .. i]
    counts <- replicateM i (uniformR (1, 3) gen)
    return $ zip roles counts

data Course = Course { availablePatterns :: [Int],
                       perClassRequirements :: [ClassRequirement],
                       numberOfClasses :: Int
                     } deriving Show

instance Generatable Course where
  generate ctx gen = do
    patterns <- filterM (const $ uniform gen) [0.. numberOfPatterns ctx - 1]
    numberOfClasses <-  uniformR (1, maxNumberOfClasses ctx) gen
    reqs <- replicateM numberOfClasses $ generate ctx gen
    return $ Course { availablePatterns = patterns,
                      perClassRequirements = reqs,
                      numberOfClasses = numberOfClasses
                    }


data Problem = Problem {
                         courses :: [Course],
                         professors :: [Professor],
                         plans :: [WeeklyPlan]
                       } deriving Show
instance Generatable Problem where
  generate ctx gen = do
    courses <- replicateM (numberOfCourses ctx) $ generate ctx gen
    professors <- replicateM (numberOfProfessors ctx) $ generate ctx gen
    plans <- replicateM (numberOfPlans ctx) $ generate ctx gen
    return $ Problem { courses = courses,
                       professors = professors,
                       plans = plans
                     }
generateCourse :: GenIO -> IO [Int]
generateCourse = \gen -> undefined

toZimpl :: Problem -> String
toZimpl p = undefined

main :: IO ()
main = return ()
