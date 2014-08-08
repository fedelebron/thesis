{-# Language TemplateHaskell #-}

import HFlags
import qualified Data.Map as M
import Control.Exception (Exception, IOException, handle)
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import Data.Char (isDigit, isSpace)
import Data.Monoid (mconcat)
import System.Exit (exitFailure)

defineFlag "facets" "facets.ieq" "Filename where facets will be found, as output by traf."
defineFlag "vertex" "vertex.v" "Filename where the fractional vertex is found, where each line contains a variable and a value. Unmentioned variable are considered 0."

type Variable = String
newtype Vertex = Vertex (M.Map Variable Double) deriving Show
newtype Facet = Facet ([(Double, Variable)], Ordering, Double)
instance Show Facet where
  show (Facet (coeffs, op, c0)) = unwords [showCoeffs, showOp op, show c0]
    where
      showCoeffs = let res = unwords $ map showTerm coeffs
                       res' = if head res == '+' then drop 2 res else res
                   in res'
      showTerm (d, v) = let sign = if d >= 0 then '+' else '-'
                            coeff = if abs d == 1 then "" else show d ++ " * "
                        in sign:' ':(coeff ++ v)
      showOp EQ = "=="
      showOp GT = ">="
      showOp LT = "<="

ecomp :: Ordering -> Double -> Double -> Bool
ecomp EQ = (==)
ecomp GT = (>=)
ecomp LT = (<=)

valid :: Vertex -> Facet -> Bool
valid v f = let Facet (coeffs, op, c0) = f
                Vertex m = v
                replace = maybe 0 id . flip M.lookup m
                res = sum [c * replace v' | (c, v') <- coeffs]
            in ecomp op res c0

readFacets :: String -> [Facet]
readFacets = catMaybes . map readFacet . drop 6 . lines

readFacet :: String -> Maybe Facet
readFacet "" = Nothing
readFacet "END" = Nothing
readFacet xs = let (coeffString, rest) = break (`elem` "=<>") xs
                   (opString, rest') = splitAt 2 rest
                   coeffs = readCoeffString coeffString
                   op = readOpString opString
                   c0 = read rest'
               in Just (Facet (coeffs, op, c0))
  where
    readOpString "==" = EQ
    readOpString "<=" = LT
    readOpString ">=" = GT

    readCoeffString [] = []
    readCoeffString (x:' ':xs) = let sign = toSign x
                                     (term, rest) = break (== ' ') xs
                                     (digits, variable) = span isDigit term
                                     coeff | null digits = 1
                                           | otherwise = read digits
                                     next = readCoeffString (tail rest)
                                 in (sign * coeff, variable) : next
      where
        toSign '+' = 1
        toSign '-' = -1
    readCoeffString xs = error $ "Tried to read: " ++ xs

readVertex :: String -> Vertex
readVertex = Vertex . mconcat . map (uncurry M.singleton . readVertexLine) . lines

readVertexLine :: String -> (Variable, Double)
readVertexLine xs = let (variable, rest) = break isSpace xs
                        coefficient = read $ dropWhile isSpace rest
                    in (variable, coefficient)

errorShowHelp :: IOException -> IO String
errorShowHelp e = do
  putStrLn $ "An exception was thrown: " ++ show e
  putStrLn "See --help for instructions."
  exitFailure
  return "Unreachable, exception thrown."

main = do
  _ <- $initHFlags "Facet validity checker"
  facetLines <- handle errorShowHelp $ readFile flags_facets
  let facets = readFacets facetLines
  vertexLines <- handle errorShowHelp $ readFile flags_vertex
  let vertex = readVertex vertexLines
      validFacet = valid vertex
      validMessage = "\x1b[32mVALID\x1b[0m:\t\t"
      invalidMessage = "\x1b[31mINVALID\x1b[0m:\t"
  forM_ facets (\f -> do
    putStr (if validFacet f then validMessage else invalidMessage)
    print f)
