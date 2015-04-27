import Distribution.Simple
import Control.Monad
import Data.Array
import System.Random

main = do
--   input <- readFile "sokal.model" >>= (return.lines)
--   let fastmodel = listArray (0,length input - 1) (map read input) :: FastModel in
    print "Hello World!"
  
type FastModel = Array Int (String,[(Int,Int)])