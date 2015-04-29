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

-- The following is simply for handling command line options.  I probably went a little overboard on getting these to work.
data Options = Options {optLength :: Int, optSeed :: Maybe Int, optShowSeed :: Bool, optHelp :: Bool} deriving (Eq,Show)
               
defaultOptions :: Options
defaultOptions = Options { optLength = 100, optSeed = Nothing, optShowSeed = False, optHelp = False}

options :: [OptDescr (Options -> Options)]
options = 
  [ Option ['l'] ["length"] 
      (ReqArg 
        (\i opts -> opts { optLength = read i }) 
        "L")
      "desired minumum length of spewed text.  Default is 100."
  , Option ['s'] ["seed"]
      (ReqArg 
        (\s opts -> opts {optSeed = Just (read s)})
        "S")
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
                    else genSpew opts

genSpew :: Options -> IO ()
genSpew opts = do
  inputModel <- readFile "sokal.model" >>= (return.lines)
  case optSeed opts of
       Just a  -> setStdGen (mkStdGen a)
       Nothing -> return ()
  gen <- getStdGen
  if optShowSeed opts
     then hPutStrLn stderr $ "Seed is " ++ show gen
     else return ()
  let fastModel = listArray (0,length inputModel - 1) (map read inputModel) :: FastModel
  let ws = evalState (runModel fastModel) gen
  print $ unwords $ takeEnough (optLength opts) ws
  
-- takeEnough: generates n words, then keeps generating words until the end of a sentence is reached.
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