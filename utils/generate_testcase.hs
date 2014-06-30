{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, TemplateHaskell #-}

import Data.List
import System.Random.MWC
import HFlags
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad (filterM, liftM, replicateM)
import qualified Data.Vector as V

defineFlag "professors" (1 :: Int) "How many professors to use"
defineFlag "courses" (1 :: Int) "How many courses to use"
defineFlag "schedules" (1 :: Int) "How many schedules to use"
defineFlag "week_days" (5 :: Int) "How many days each week has"
defineFlag "weeks" (2 :: Int) "How many weeks to consider"
defineFlag "roles" (5 :: Int) "How many roles to use"
defineFlag "classes" (20 :: Int) "Maximum number of classes per course"
defineFlag "course_schedules" (2 :: Int) "How many schedules each course can use"
defineFlag "start_dates" (5 :: Int) "How many starting dates to use"
defineFlag "course_start_dates" (2 :: Int) "How many starting dates to use"
defineFlag "max_roles" (2 :: Int) "How many instances of a given role may be required for a class as a maximum"
defineFlag "min_roles" (0 :: Int) "How many instances of a given role may be required for a class as a minimum"
defineFlag "max_p" (10 :: Int) "Maximum number of classes a professor can be allowed to give, at the most"
defineFlag "max_start_date" (2 :: Int) "The latest week a course can start at"
defineFlag "random_seed" (0 :: Int) "A random seed for the testcase, if none is specified, a fresh one will be generated"
defineFlag "availability_probability" (0.5 :: Double) "The probability a professor is available on a given day"

data Context = Context { numberOfProfessors :: Int,
                         numberOfDays :: Int,
                         numberOfCourses :: Int,
                         numberOfSchedules :: Int,
                         numberOfCourseSchedules :: Int,
                         numberOfWeeks :: Int,
                         numberOfWeekDays :: Int,
                         numberOfRoles :: Int,
                         numberOfStartDates :: Int,
                         numberOfCourseStartDates :: Int,
                         maxNumberOfRoles :: Int,
                         minNumberOfRoles :: Int,
                         maxMaxP :: Int,
                         maxNumberOfClasses :: Int,
                         maxStartDate :: Int,
                         availabilityProbability :: Double,
                         randomSeed :: Seed
} deriving Show

class Generatable t where
  generate :: PrimMonad m => Context -> Gen (PrimState m) -> m t

type Role = Int
instance Generatable Role where
  generate ctx = uniformR (1, numberOfRoles ctx)

data Professor = Professor { availability :: [Bool],
                             roles :: [Role],
                             quality :: Course -> Int,
                             maxP :: Int
                           }
instance Show Professor where
  show p = let available = let pairs = zip [1..] (availability p)
                           in  show . map fst $ filter snd pairs
               r = show $ roles p
               mp = show $ maxP p
           in "{roles = " ++ r ++ ", available = " ++ available ++ ", maxp = " ++ mp ++ "}"

instance Generatable Professor where
  generate ctx gen = do
    let f = do
          x <- uniform gen
          return $ x < availabilityProbability ctx
    availability <- replicateM (numberOfDays ctx) f
    i <- uniformR (0, numberOfRoles ctx - 1) gen
    mp <- uniformR (1, maxMaxP ctx) gen -- at least one teachable class per professor
    let roles = [0 .. i]
    let quality = const 8
    return $ Professor { availability = availability,
                         roles = roles,
                         quality = quality,
                         maxP = mp}

type Schedule = [Int]
instance Generatable Schedule where
  generate ctx gen = filterM (const $ uniform gen) [0.. (numberOfWeekDays ctx - 1)]

type ClassRequirement = [(Role, (Int, Int))]
instance Generatable ClassRequirement where
  generate ctx gen = do
    i <- uniformR (0, numberOfRoles ctx - 1) gen
    let roles = [0 .. i]
    let rnd = replicateM (i-1) (uniformR (minNumberOfRoles ctx, maxNumberOfRoles ctx) gen)
    counts1 <- rnd
    counts2 <- rnd
    let sorted = map (\(x, y) -> (min x y, max x y)) (zip counts1 counts2)
        sorted' = sorted ++ [(1, 1)] -- this is to ensure there are
                                     -- no totally empty requirements
    return $ zip roles sorted'

data Course = Course { availableSchedules :: [Int],
                       availableStartingWeeks :: [Int],
                       perClassRequirements :: [ClassRequirement],
                       numberOfClasses :: Int
                     } deriving Show

instance Generatable Course where
  generate ctx gen = do
    schedules <- filterM (const $ uniform gen) [0.. numberOfCourseSchedules ctx - 1]
    schedules' <- if null schedules -- at least one schedule per course
                  then do
                    x <- uniformR (0, numberOfCourseSchedules ctx - 1) gen
                    return [x]
                  else return schedules
    startingWeeks <- filterM (const $ uniform gen) [0 .. maxStartDate ctx ]
    let startingWeeks' = 0:startingWeeks -- arbitrarily add the first week as a possible starting week
    numberOfClasses <-  uniformR (1, maxNumberOfClasses ctx) gen
    reqs <- replicateM numberOfClasses $ generate ctx gen
    return $ Course { availableSchedules = schedules',
                      availableStartingWeeks = startingWeeks',
                      perClassRequirements = reqs,
                      numberOfClasses = numberOfClasses
                    }

data Problem = Problem {
                         context :: Context,
                         courses :: [Course],
                         professors :: [Professor],
                         schedules :: [Schedule],
                         startingWeeks :: [Int]
                       } deriving Show
instance Generatable Problem where
  generate ctx gen = do
    courses <- replicateM (numberOfCourses ctx) $ generate ctx gen
    professors <- replicateM (numberOfProfessors ctx) $ generate ctx gen
    schedules <- replicateM (numberOfSchedules ctx) $ generate ctx gen
    startingWeeks <- replicateM (numberOfStartDates ctx) $ generate ctx gen
    let startingWeeks' = filter (<= maxStartDate ctx) startingWeeks
    if (length startingWeeks' /= numberOfStartDates ctx)
    then generate ctx gen
    else return $ Problem { context = ctx,
                            courses = courses,
                            professors = professors,
                            schedules = schedules,
                            startingWeeks = startingWeeks'
                          }

showA :: Problem -> String
showA p = "param a[P * D] :=    | " ++ intercalate ", " (map show [0 .. numberOfDays (context p) - 1]) ++ " |\n"
          ++ intercalate "\n" (zipWith showProf [0..] (professors p)) ++ ";"
  where
    showProf k p = "                   |" ++ show k ++ "| " ++ intercalate ", " (map (show . fromEnum) $ availability p) ++ " |"

showR :: Problem -> String
showR p = "param r[P * R] :=    | " ++ intercalate ", " (map show [0 .. n]) ++ " |\n"
          ++ intercalate "\n" (zipWith showProf [0..] (professors p)) ++ ";"
  where
    showProf k p = "                   |" ++ show k ++ "| " ++ intercalate ", " (map (show . fromEnum . (`elem` roles p)) [0 .. n]) ++ " |"
    n = numberOfRoles (context p) - 1

showQ :: Problem -> String
showQ p = "param q[P * C] :=    | " ++ intercalate ", " (map show [0 .. n - 1]) ++ " |\n"
          ++ intercalate "\n" (zipWith showProf [0..] (professors p)) ++ ";"
  where
    showProf k p = "                   |" ++ show k ++ "| " ++ intercalate ", " (map (show . quality p) cs) ++ " |"
    cs = courses p
    n = length cs

showN :: Problem -> String
showN p = "param n[C] := " ++ intercalate ", " (zipWith f [0..] cs) ++ ";"
  where
    cs = courses p
    f k c = '<' : show k ++ "> " ++ show (numberOfClasses c)

showPs :: Show a => String -> String -> (Problem -> [a]) -> (Course -> [Int]) -> Problem -> String
showPs name name' u s p = "param p" ++ name ++ "[C * " ++ name' ++"] :=    | " ++ intercalate ", " (map show [0 .. n - 1]) ++ " |\n"
           ++ intercalate "\n" (zipWith showCourse [0..] (courses p)) ++ ";"
  where
    ss = u p
    n = length ss
    showCourse :: Int -> Course -> String
    showCourse k c = "                    |" ++ show k ++ "| " ++ intercalate ", " (map f [0 .. n - 1]) ++ " |"
      where
        f = show . fromEnum . (`elem` s c)

showm :: ((Int, Int) -> Int) -> String -> Problem -> String
showm selector name p = "param " ++ name ++ "[C * L_ * R] :=       | " ++ intercalate ", " (map show [0 .. r - 1]) ++ " |\n"
          ++ intercalate "\n" [f (c, k) l | (c, k) <- zip cs [0..], l <- ls c] ++ ";"
  where
    r = numberOfRoles (context p)
    cs = courses p
    ls x = [0 .. numberOfClasses x - 1]
    f (c, i) l = "                        |" ++ show i ++ ", " ++ show l ++ "| " ++ intercalate ", " (map g [0 .. r - 1]) ++ " |"
      where
        g k = let reqs = perClassRequirements c !! l
              in maybe "0" (show . selector) (lookup k reqs)

showDS :: Problem -> String
showDS p = "param ds[S * SD *  L_ * D] :=    | " ++ intercalate ", " (map show [0 .. d - 1]) ++ " |\n"
           ++ intercalate "\n" [g k | (i, s) <- zip [0..] (schedules p),
                                      (j, sd) <- zip [0..] (startingWeeks p),
                                      let g = f (i, s) (j, sd),
                                      k <- [0 .. n - 1]]
           ++ ";"
  where
    d = numberOfDays (context p)
    n = maxNumberOfClasses (context p)
    f (i, s) (j, sd) = g where
      classDays = toYearlyDates sd s
      g k = "                         |" ++ show i ++ ", " ++ show j ++ ", " ++ show k ++ "| " ++ str
        where
          idx = if k >= length classDays -- this class cannot be scheduled within the semester
                then -1
                else classDays !! k
          v | idx == -1 = replicate d 0
            | otherwise = replicate idx 0 ++ [1] ++ replicate (d - idx - 1) 0 :: [Int]
          str = intercalate ", " (map show v) ++ " |"
    toYearlyDates :: Int -> Schedule -> [Int]
    toYearlyDates startWeek = takeWhile (< d) . map (+ offset) . concat . zipWith map (map (+) [0, n'..]) . repeat
      where
        n' = numberOfWeekDays (context p)
        m = n' * numberOfWeeks (context p)
        offset = startWeek * n'

showMaxP :: Problem -> String
showMaxP p = "param maxp[P] := " ++ intercalate ", " (zipWith f [0..] ps) ++ ";"
  where
    ps = professors p
    f k p = '<' : show k ++ "> " ++ show (maxP p)

setShower :: String -> (Context -> Int) -> Problem -> String
setShower str f p = "set " ++ str ++ " := { 0 .. " ++ show (f (context p) - 1) ++ " };"

showL :: Problem -> String
showL p = "param L := " ++ show (maxNumberOfClasses (context p)) ++ ";"

toZimpl :: Problem -> String
toZimpl p = unlines . map ($ p) $ [setShower "P" numberOfProfessors,
                                   setShower "R" numberOfRoles,
                                   setShower "D" numberOfDays,
                                   setShower "C" numberOfCourses,
                                   setShower "S" numberOfSchedules,
                                   setShower "SD" numberOfStartDates,
                                   showL,
                                   setShower "L_" maxNumberOfClasses,
                                   showA,
                                   showR,
                                   showQ,
                                   showN,
                                   showm fst "m",
                                   showm snd "M",
                                   showPs "p" "S" schedules availableSchedules,
                                   showPs "sd" "SD" startingWeeks availableStartingWeeks,
                                   showMaxP,
                                   showDS]

createContext :: Context
createContext = Context {
  numberOfProfessors = flags_professors,
  numberOfWeeks = flags_weeks,
  numberOfCourses = flags_courses,
  numberOfSchedules = flags_schedules,
  numberOfCourseSchedules = min flags_course_schedules flags_schedules,
  numberOfRoles = flags_roles,
  numberOfWeekDays = flags_week_days,
  numberOfStartDates = flags_start_dates,
  numberOfDays = flags_week_days * flags_weeks,
  numberOfCourseStartDates = min flags_course_start_dates flags_start_dates,
  maxNumberOfRoles = flags_max_roles,
  minNumberOfRoles = flags_min_roles,
  maxMaxP = flags_max_p,
  maxNumberOfClasses = flags_classes,
  availabilityProbability = flags_availability_probability,
  maxStartDate = min flags_max_start_date flags_weeks,
  randomSeed = toSeed (V.singleton 0)
}

main :: IO ()
main = do
  s <- $initHFlags "Test case generator"
  seed <- if flags_random_seed == 0
          then withSystemRandom (asGenIO save)
          else return . toSeed . V.singleton . toEnum $ flags_random_seed
  let ctx = createContext { randomSeed = seed }
  x <- restore (randomSeed ctx)
  p :: Problem <- generate ctx x
  putStrLn (toZimpl p)
  readFile "constraints.txt" >>= putStrLn
