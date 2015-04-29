import Distribution.Simple
import Control.Monad
import Control.Monad.State.Lazy
import System.IO
import Data.Array
import System.Random
import System.Environment
import Data.List
import System.Console.GetOpt
import Data.Maybe

-- The following is simply for handling command line options.  I probably went a little overboard.
-- This is taken from 
data Options = Options {optLength :: Int, optSeed :: Maybe Int, optHelp :: Bool} deriving (Eq,Show)
               
defaultOptions :: Options
defaultOptions = Options { optLength = 100, optSeed = Nothing, optHelp = False}

options :: [OptDescr (Options -> Options)]
options = 
  [ Option ['l'] ["length"] 
      (ReqArg 
        (\i opts -> opts { optLength = read i }) 
        "L")
      "desired length of spewed text"
  , Option ['s'] ["seed"]
      (ReqArg 
        (\s opts -> opts {optSeed = Just (read s)})
        "S")
      "user-supplied seed, if the user so desires"
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
                    else genSpew (optLength opts) (optSeed opts)

genSpew :: Int -> Maybe Int -> IO ()
genSpew len seed = do
  inputModel <- readFile "sokal.model" >>= (return.lines)
  case seed of
       Just a -> setStdGen (mkStdGen a)
  gen <- getStdGen
  let fastModel = listArray (0,length inputModel - 1) (map read inputModel) :: FastModel
  let ws = evalState (runModel fastModel) gen
  print $ intercalate " " $ takeEnough len ws
  
takeEnough :: Int -> [String] -> [String]
takeEnough n (x:xs)
  | n > 0     = x:(takeEnough (n-1) xs)
  | otherwise = takeUntilEndOfSentence (x:xs) where
    takeUntilEndOfSentence (x:xs)
      | last x == '.' = [x]
      | otherwise     = x:(takeUntilEndOfSentence xs)

-- FastModel at index i is an ordered pair containing the encoding string of i and a list of ordered pairs of ints
-- of the form (weight,another index or -1)
type FastModel = Array Int (String,[(Int,Int)])

type RandState = State StdGen

-- This is heavily inspired (read: copied verbatim then modified) from Lecture 18 from 16100.
markovSelect :: [(Int,Int)] -> RandState Int
markovSelect successors = fmap (weightedSelect successors) . state . randomR $ (0,sum $ (map fst) successors) where
  weightedSelect ((weight,state):remainders) ix
    | ix - weight <= 0   = state
    | otherwise = weightedSelect remainders (ix-weight)

-- So is this.
runModel :: FastModel -> RandState [String]
runModel fastmodel = do
  start <- state . randomR $ bounds fastmodel
  iter start where
    iter ix = do
      let (ixString,succs) = fastmodel ! ix
      succ <- markovSelect succs
      case succ of
        -1 -> do
          rest <- (state . randomR) (bounds fastmodel) >>= (\s -> iter s)
          return $ ixString:rest
        n  -> do
          rest <- iter n
          return $ ixString:rest
        
-- Ibid.
linefill :: Int -> [String] -> String
linefill _ [] = "\n"
linefill n (x:xs) = iter x xs where
  iter x (y:ys)
    | length x + length y + 1 > n = x ++ "\n" ++ linefill n (y:ys)
    | otherwise                   = iter (x ++ " " ++ y) ys
  iter x [] = x ++ "\n"

 
