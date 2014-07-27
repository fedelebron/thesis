import System.Process (waitForProcess,
                       runInteractiveProcess,
                       readProcessWithExitCode)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn,
                  stderr,
                  stdout,
                  BufferMode(NoBuffering, LineBuffering),
                  hSetBinaryMode,
                  hSetBuffering,
                  hGetContents)
import System.Environment (getArgs, getProgName)
import System.FilePath.Posix (addExtension, replaceExtension)
import System.Posix.Files (rename)
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import Control.Monad (when)
import GHC.IO.Handle (Handle)
import System.ProgressBar (progressBar, noLabel, percentage)
import Data.List (isPrefixOf)
import Data.Char (isSpace, isDigit)

import Debug.Trace

printHelp :: IO ()
printHelp = do
  progName <- getProgName
  let str = "Usage: " ++ progName ++ " problem.lp"
  hPutStrLn stderr str

type TranslationMap = M.Map String String

createTranslationMap :: String -> TranslationMap
createTranslationMap str = let equations = drop 9 (lines str)
                               splitLines = map words equations
                               pairs = filter ((== 2) . length) splitLines
                               tuples = map (swap . tuplify) pairs
                           in foldr (uncurry M.insert) M.empty tuples
  where
    tuplify :: [a] -> (a, a)
    tuplify [x, y] = (x, y)

type Sign = Char
type Variable = String
type Term = (Sign, Variable)

translateFacets :: TranslationMap -> String -> String
translateFacets t = unlines . map translateLine . lines
  where
    translateLine :: String -> String
    translateLine [] = []
    translateLine f@('(':_) = translateFacet f
    translateLine x = x

    translateFacet :: String -> String
    translateFacet f = let f' = tail . dropWhile ((/=) ')') $ f
                           terms = toTerms f'
                           strings = map (uncurry (:)) terms
                        in unwords strings
      where
        toTerms :: String -> [Term]
        toTerms [] = []
        toTerms (' ':xs) = toTerms xs
        toTerms (x:xs) | x `elem` "+-" = let (var, rest) = break (`elem` " +-") xs
                                             Just v = M.lookup var t
                                         in (x, v) : toTerms rest
        toTerms (x:xs) | x `elem` "<>=" = [(x, xs)]

reportPORTAProgress :: Handle -> IO ()
reportPORTAProgress handle = do
  hSetBinaryMode handle False
  hSetBuffering handle LineBuffering
  contents <- hGetContents handle

  hSetBuffering stdout NoBuffering

  let li = lines contents

  putStrLn "Gaussian elimination..."
  li' <- track "GAUSS" li
  putStrLn "\nGaussian elimination done."

  putStrLn "Fourier-Motzkin..."
  li'' <- track "FOURIER - MOTZKIN" li'
  putStrLn "\nFourier-Motzkin done."

  where
    bar = progressBar percentage percentage 100
    track str = countdownStart
                . drop 5
                . dropWhile (not . isPrefixOf str)
    countdownStart (x:xs) = let n = read
                                    . takeWhile isDigit
                                    . dropWhile isSpace
                                    . tail
                                    $ x
                            in countdown n n xs
    countdown _ 1 xs = do
      bar 1 1
      return xs
    countdown n k (('|':_):xs) = do
      bar (n - k) n
      countdown n (k - 1) xs
    countdown n k (x:xs) = countdown n k xs

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ printHelp >> exitFailure
  let (lpFile:_) = args
  -- The file where the vertices will be written to
  let poiFile = replaceExtension lpFile "poi"
  -- The file where the translation table for the lp variables will be
  -- written to
  let tblFile = replaceExtension lpFile "tbl"

  -- -t to print the translation table to stderr
  -- -a because our variables are arbitrarily named, not x1, ..., xn
  let params = ["-t", "-a", lpFile]
  (status, stdout, stderr) <- readProcessWithExitCode "zerOne" params ""

  putStrLn $ "Writing " ++ (show $ length (lines stdout) - 5) ++ " vertices..."
  writeFile poiFile stdout
  putStrLn "Writing translation table..."
  writeFile tblFile stderr

  let translationMap = createTranslationMap stderr

  let params = ["-T", "-l", poiFile]
  --  (status, stdout, stderr) <- readProcessWithExitCode "./xporta" params ""
  (stdin, stdout, stderr, proc) <- runInteractiveProcess "./xporta" params Nothing Nothing
  -- Set to text mode

  reportPORTAProgress stdout

  waitForProcess proc
  -- Where PORTA writes the facets to
  let ieqFile' = addExtension poiFile "ieq"
      ieqFile = replaceExtension poiFile "ieq"
  rename ieqFile' ieqFile

  inequalities <- readFile ieqFile

  let translated = translateFacets translationMap inequalities
  let txtFile = replaceExtension lpFile "txt"
  writeFile txtFile translated

  exitSuccess
