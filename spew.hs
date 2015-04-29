import Distribution.Simple
import Control.Monad
import Control.Monad.State.Lazy
import System.IO
import Data.Array
import System.Random
import System.Environment
import System.Exit

main = do
  {-putStrLn "Seed? Press enter for default."
  seedIn <- getLine
  case seedIn of
       [] -> return ()
       _  -> setStdGen $ read seedIn
  putStrLn "Number of words? Default is 100."
  wordCount <- getLine-}
  inputModel <- readFile "sokal.model" >>= (return.lines)
  setStdGen (mkStdGen 0) --debugging to ensure a consistent random number generator
  gen <- getStdGen
  let fastModel = listArray (0,length inputModel - 1) (map read inputModel) :: FastModel
  let ws = evalState (runModel fastModel) gen
  print $ linefill 100 ws
  
-- FastModel at index i is an ordered pair containing the encoding string of i and a list of ordered pairs of ints
-- of the form (weight,another index or -1)
type FastModel = Array Int (String,[(Int,Int)])

type RandState = State StdGen

-- This is heavily inspired (read: copied verbatim then modified) from Lecture 18 from 16100.
markovSelect :: [(Int,Int)] -> RandState Int
markovSelect successors = fmap (weightedSelect successors) . state . randomR $ (0,sum $ (map fst) successors) where
  weightedSelect ((weight,state):remainders) ix
    | ix <= 0   = state
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

 
