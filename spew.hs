import Distribution.Simple
import System.Random
import System.Environment
import System.Console.GetOpt
import System.IO
import Control.Monad.State.Lazy
import SpewUtils
import Data.Char
import System.Exit

-- |The following is simply for handling command line options.  I probably went a little overboard on getting these to work.
data Options = Options {optLength :: String, optSeed :: Maybe String, optShowSeed :: Bool, optHelp :: Bool} deriving (Eq,Show)
               
-- |defaultOptions: the default program options.
defaultOptions :: Options
defaultOptions = Options { optLength = "100", optSeed = Nothing, optShowSeed = False, optHelp = False}

-- |options mapping.
options :: [OptDescr (Options -> Options)]
options = 
  [ Option ['l'] ["length"] 
      (ReqArg 
        (\i opts -> opts { optLength = i }) 
        "m")
      "desired minumum length of spewed text.  Default is 100."
  , Option ['s'] ["seed"]
      (ReqArg 
        (\s opts -> opts {optSeed = Just s})
        "n")
      "for the user to supply their own seed for the random number generator, if so desired"
  , Option ['o'] ["show-seed"]
      (NoArg
        (\opts -> opts {optShowSeed = True}))
      "show the value of the generator seed.  Default is suppressing the generator seed."
  , Option ['h'] ["help"]
      (NoArg
        (\opts -> opts {optHelp = True}))
      "show this help text"
  ]

dumpHelp :: IO ()
dumpHelp = do
  prg <- getProgName
  hPutStrLn stderr (usageInfo prg options)
  
main = do   
  (actions,_,_) <- getArgs >>= return . (getOpt Permute options)
  -- this may terminate the program if the optHelp flag has been set
  let opts = foldr id defaultOptions actions
  if (optHelp opts) then dumpHelp
                    else sanityCheckLength opts >> genSpew opts

sanityCheckLength :: Options -> IO ()
sanityCheckLength opts = if and $ (map isDigit) (optLength opts)
                            then return ()
                            else hPutStrLn stderr "Error: length should be an integer." >> (exitWith $ ExitFailure 1)
    
                    
genSpew :: Options -> IO ()
genSpew opts = do
  inputModel <- readFile "sokal.model" >>= (return.lines)
  case optSeed opts of
       Just a  -> setStdGen (read a)
       Nothing -> return ()
  gen <- getStdGen
  if optShowSeed opts
     then hPutStrLn stderr $ "Seed is " ++ show gen
     else return ()
  let ws = evalState (SpewUtils.runModel inputModel) gen
  -- the entire purpose of capitalizeBeginning is because I got really tired of seeing the first letter of output in lower case.
  putStr $ capitalizeBeginning (linefill 72  (takeEnough (read (optLength opts)) ws)) where
     capitalizeBeginning = zipWith ($) (toUpper:(repeat id))

-- |takeEnough: generates n words, then keeps generating words until the end of a sentence is reached.
takeEnough :: Int -> [String] -> [String]
takeEnough n (x:xs)
  | n > 0     = x:(takeEnough (n-1) xs)
  | otherwise = takeUntilEndOfSentence (x:xs) where
    takeUntilEndOfSentence (x:xs)
      | last x == '.' = [x]
      | otherwise     = x:(takeUntilEndOfSentence xs)
        
-- |linefill: for avoiding lines that are 10,000 words long.
-- Ripped intact from the CS 16100 notes.
linefill :: Int -> [String] -> String
linefill _ [] = "\n"
linefill n (x:xs) = iter x xs where
  iter x (y:ys)
    | length x + length y + 1 > n = x ++ "\n" ++ linefill n (y:ys)
    | otherwise                   = iter (x ++ " " ++ y) ys
  iter x [] = x ++ "\n"