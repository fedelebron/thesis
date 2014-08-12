{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

import System.Random.MWC
import HFlags
import qualified Data.Vector as V
import System.IO (hPutStrLn, stderr)

import Context
import Problem

defineFlag "professors" (1 :: Int) "How many professors to use"
defineFlag "courses" (1 :: Int) "How many courses to use"
defineFlag "schedules" (1 :: Int) "How many schedules to use"
defineFlag "week_days" (5 :: Int) "How many days each week has"
defineFlag "weeks" (2 :: Int) "How many weeks to consider"
defineFlag "roles" (5 :: Int) "How many roles to use"
defineFlag "classes" (20 :: Int) "Maximum number of classes per course"
defineFlag "course_schedules" (2 :: Int) "How many schedules each course can use"
defineFlag "start_weeks" (5 :: Int) "How many starting weeks to use"
defineFlag "course_start_weeks" (2 :: Int) "How many starting weeks to use per course"
defineFlag "max_roles" (2 :: Int) "How many instances of a given role may be required for a class as a maximum"
defineFlag "min_roles" (0 :: Int) "How many instances of a given role may be required for a class as a minimum"
defineFlag "max_p" (10 :: Int) "Maximum number of classes a professor can be allowed to give, at the most"
defineFlag "max_start_week" (2 :: Int) "The latest week a course can start at"
defineFlag "random_seed" (0 :: Int) "A random seed for the testcase, if none is specified, a fresh one will be generated"
defineFlag "availability_probability" (0.5 :: Double) "The probability a professor is available on a given day"

createContext :: Context
createContext = Context {
  numberOfProfessors = flags_professors,
  numberOfWeeks = flags_weeks,
  numberOfCourses = flags_courses,
  numberOfSchedules = flags_schedules,
  numberOfCourseSchedules = min flags_course_schedules flags_schedules,
  numberOfRoles = flags_roles,
  numberOfWeekDays = flags_week_days,
  numberOfStartWeeks = flags_start_weeks,
  numberOfDays = flags_week_days * flags_weeks,
  numberOfCourseStartWeeks = min flags_course_start_weeks flags_start_weeks,
  maxNumberOfRoles = flags_max_roles,
  minNumberOfRoles = flags_min_roles,
  maxMaxP = flags_max_p,
  maxNumberOfClasses = flags_classes,
  availabilityProbability = flags_availability_probability,
  maxStartWeek = min flags_max_start_week (flags_weeks - 1)
}

dimension_bound :: Problem -> Int
dimension_bound problem = let ctx = context problem
                              ns = map numberOfClasses $ courses problem
                              k = sum ns
                              t = sum (map (length . filter id . availability) (professors problem))
                              q = sum (map (length . roles) (professors problem))
                              s = numberOfDays ctx
                              n = maximum ns
                              p = numberOfProfessors ctx
                              c = numberOfCourses ctx
                              r = numberOfRoles ctx
                              m = numberOfSchedules ctx
                              w = numberOfWeeks ctx
                    in k * (p * r + s - 1) - p * s + t + c * (m + w + n * q - n * p * r + p * s - 2)

main :: IO ()
main = do
  s <- $initHFlags "Test case generator"
  seed <- if flags_random_seed == 0
          then withSystemRandom (asGenIO save)
          else return . toSeed . V.singleton . toEnum $ flags_random_seed
  let ctx = createContext
  x <- restore seed
  p :: Problem <- generate ctx x
  putStrLn (toZimpl p)
  readFile "constraints.txt" >>= putStrLn
  putStrLn $ "# Dimension bound: " ++ (show $ dimension_bound p)
