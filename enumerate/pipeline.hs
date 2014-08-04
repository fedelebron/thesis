import System.Process (spawnProcess,
                       readProcess,
                       waitForProcess,
                       runInteractiveProcess,
                       readProcessWithExitCode,
                       proc,
                       StdStream(UseHandle),
                       createProcess,
                       std_out,
                       std_err)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn,
                  stderr,
                  stdout,
                  BufferMode(NoBuffering, LineBuffering),
                  hSetBinaryMode,
                  hSetBuffering,
                  hGetContents,
                  openFile,
                  hFlush,
                  IOMode(WriteMode))
import System.Environment (getArgs, getProgName)
import System.FilePath.Posix (addExtension, replaceExtension)
import System.Posix.Files (rename)
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import Control.Monad (when, forever)
import GHC.IO.Handle (Handle)
import System.ProgressBar (progressBar, noLabel, percentage)
import Data.List (isPrefixOf)
import Data.Char (isSpace, isDigit)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Control.Concurrent (forkIO, threadDelay, killThread)

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

type Term = String

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
                        in unwords terms
      where
        toTerms :: String -> [Term]
        toTerms [] = []
        toTerms (' ':xs) = toTerms xs
        toTerms (x:xs) | x `elem` "+-" = let trimmed = dropWhile isSpace xs
                                             (nvar, rest) = break (`elem` " +-") trimmed
                                             (coeff, var) = span isDigit nvar
                                             v = maybe (error $ "Variable not found: " ++ var) id $ M.lookup var t
                                         in (x:' ':(coeff ++ v)) : toTerms rest
        toTerms (x:'=':xs) = [x:'=':' ':dropWhile isSpace xs]

reportPORTAProgress :: Handle -> IO ()
reportPORTAProgress handle = do
  hSetBinaryMode handle False
  hSetBuffering handle LineBuffering
  hSetBuffering stdout NoBuffering

  contents <- hGetContents handle
  let li = lines contents

  putStrLn "Gaussian elimination..."
  li' <- track "GAUSS" li
  putStrLn "\nGaussian elimination done."

  putStrLn "Fourier-Motzkin..."
  li'' <- track "FOURIER - MOTZKIN" li'
  putStrLn "\nFourier-Motzkin done."

  let written = "output written" `isPrefixOf` last (init li'')
  let str = if written
            then "Facets written to disk."
            else "Facets not written to disk."
  when (not written) $ do
    putStrLn "Last 10 lines of output:"
    let r = reverse . take 10 $ reverse li''
    print r
  putStrLn str

  where
    bar :: Integer -> Integer -> IO ()
    bar = progressBar percentage percentage 100
    track :: String -> [String] -> IO [String]
    track str = countdownStart
                . drop 5
                . dropWhile (not . isPrefixOf str)
    countdownStart :: [String] -> IO [String]
    countdownStart (x:xs) = let n = read
                                    . takeWhile isDigit
                                    . dropWhile isSpace
                                    . tail
                                    $ x
                            in countdown n n xs
    countdown :: Integer -> Integer -> [String] -> IO [String]
    countdown _ 1 xs = bar 1 1 >> return xs
    countdown n k (('|':_):xs) = do
      bar (n - k) n
      countdown n (k - 1) xs
    countdown n k (x:xs) = countdown n k xs

checkVertexFileStatus :: String -> UTCTime -> IO ()
checkVertexFileStatus fileName startTime = do
  stdout' <- readProcess "wc" ["-l", "-c", fileName] ""
  let [lines, chars, _] = words stdout'
  let nlines = read lines :: Int
      nchars = read chars :: Int
      mb = nchars `div` (1024^2)
  currTime <- getCurrentTime
  let diff = toInteger . round $ currTime `diffUTCTime` startTime
  putStr "\r                                                                    \r"
  putStr $ (show nlines) ++ "\t\t" ++ (show mb) ++ "MB\t\t+" ++ (show diff) ++ "s"
  hFlush stdout

periodically :: IO () -> IO ()
periodically action = threadDelay 100000 >> action >> periodically action

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ printHelp >> exitFailure
  let (lpFile:_) = args

  -- Clean .lp by removing useless variables
  let cleanedLpFile = replaceExtension lpFile "cleaned.lp"
  cleanedHandle <- openFile cleanedLpFile WriteMode
  let params = ["--in", "lp", "--out", "lp", "--clean", lpFile]
  let cmd = "./enumerate"
  let process = (proc cmd params) { std_out = UseHandle cleanedHandle }
  (_, _, _, processHandle) <- createProcess process
  waitForProcess processHandle

  -- The file where the vertices will be written to
  let poiFile = replaceExtension lpFile "poi"
  poiHandle <- openFile poiFile WriteMode
  -- The file where the translation table for the lp variables will be
  -- written to
  let tblFile = replaceExtension lpFile "tbl"
  tblHandle <- openFile tblFile WriteMode

  putStrLn "Vertices\tFile size\tTime"
  currentTime <- getCurrentTime
  checker <- forkIO (periodically (checkVertexFileStatus poiFile currentTime))

  -- -t to print the translation table to stderr
  -- -a because our variables are arbitrarily named, not x1, ..., xn
  let params = ["-t", "-a", cleanedLpFile]
  let process = (proc "zerOne" params) { std_out = UseHandle poiHandle,
                                         std_err = UseHandle tblHandle }
  (_, _, _, processHandle) <- createProcess process
  waitForProcess processHandle

  killThread checker

  translationTable <- readFile tblFile
  let translationMap = createTranslationMap translationTable
  let params = ["-T", "-l", poiFile]
  let cmd = "./xporta"
  (_, stdout, _, proc) <- runInteractiveProcess cmd params Nothing Nothing

  putStrLn ""
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
