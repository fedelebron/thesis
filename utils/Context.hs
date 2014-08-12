module Context where

import System.Random.MWC (Gen)
import Control.Monad.Primitive (PrimMonad, PrimState)
data Context = Context { numberOfProfessors :: Int,
                         numberOfCourses :: Int,
                         numberOfSchedules :: Int,
                         numberOfCourseSchedules :: Int,
                         numberOfWeeks :: Int,
                         numberOfWeekDays :: Int,
                         numberOfRoles :: Int,
                         numberOfStartWeeks :: Int,
                         numberOfCourseStartWeeks :: Int,
                         maxNumberOfRoles :: Int,
                         minNumberOfRoles :: Int,
                         maxMaxP :: Int,
                         maxNumberOfClasses :: Int,
                         maxStartWeek :: Int,
                         availabilityProbability :: Double
} deriving Show

numberOfDays :: Context -> Int
numberOfDays c = numberOfWeeks c * numberOfWeekDays c

class Generatable t where
  generate :: PrimMonad m => Context -> Gen (PrimState m) -> m t

