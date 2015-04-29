module SpewUtils (runModel) where
import Data.Array
import System.Random
import Control.Monad.State.Lazy

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
runModel :: [String] -> RandState [String]
runModel inputModel = do
  start <- state . randomR $ bounds fastModel
  iter start where
    fastModel = listArray (0,length inputModel - 1) (map read inputModel)
    iter ix = do
      let (ixString,succs) = fastModel ! ix
      succ <- markovSelect succs
      case succ of
        -1 -> do
          rest <- (state . randomR) (bounds fastModel) >>= (\s -> iter s)
          return $ ixString:rest
        n  -> do
          rest <- iter n
          return $ ixString:rest