module SuckUtils (processAndSerialize) where

import qualified Data.Map as M
import Data.List
import Data.Maybe

type PrimitiveModel = M.Map (String,String) [String]

type IntermediaryModel = M.Map (String,String) [(Int,String)]

type ProcessedModel = [(String,[(Int,Int)])]
 
-- |processAndSerialize: processes input text into a ProcessedModel, and serializes the result.
processAndSerialize :: [String] -> String
processAndSerialize = serialize . process

-- |process: big catch-all function for processing data.  Takes nicely formatted text and builds a processed model.
process :: [String] -> ProcessedModel
process = toProcessedModel . toIntermediaryModel . toPrimitiveModel

-- |toPrimitiveModel: takes webpage text stripped of its HTML tags and spits out a primitive model.
toPrimitiveModel :: [String] -> PrimitiveModel
toPrimitiveModel s = prim_model where
  -- turns each paper in s into a list of words
  stage1 = map words s
  -- for each list of words w in stage1, builds a list of elements of the form ((x,y),[z]) whenever the pair of words x,y in w is followed by z.
  -- using [z] instead of z is necessary for building the primitive model.
  -- The double application of tail will cause problems when someone writes a postmodernist paper with only one word in it.
  paperwiseAssocs = map (\x -> (x `zip` tail x) `zip` (map (\a -> [a]) ((tail (tail x))))) stage1
  -- combines the paperwise associations into a giant list of associations to build into a primitive model.
  assocs = concat paperwiseAssocs
  -- builds a primitive model from assocs.  Use of fromListWith (++) assures that whenever assocs contains ((x,y),[z]) 
  -- and ((x,y),[t]), prim_model will contain value [z,t...] at key (x,y).
  prim_model = M.fromListWith (++) assocs
  
-- |toIntermediaryModel: Takes a primitive model and spits out an intermediary model.
toIntermediaryModel :: PrimitiveModel -> IntermediaryModel
-- Counting the frequency of an element in a list is much easier when that list is sorted.
toIntermediaryModel = M.map (freqCount.sort) where
  -- freqCount assumes that its second argument is sorted
  freqCount = partialFreqCount [] where
    partialFreqCount a [] = a
    partialFreqCount [] (b:bs) = partialFreqCount [(1,b)] bs
    partialFreqCount ((n,a):as) (b:bs)
      | a == b    = partialFreqCount ((n+1,a):as) bs
      | otherwise = partialFreqCount ((1,b):(n+1,a):as) bs
      
-- |toProcessedModel: Takes an IntermediaryModel and spits out a ProcessedModel.
-- The location of a string pair (x,y) in the ordering of the keys of the IntermediaryModel 
-- will determine its id.
toProcessedModel :: IntermediaryModel -> ProcessedModel
toProcessedModel interm = zip (map snd indices) (M.elems encodedMap) where
  -- indices is a list of keys of interm in ascending order
  indices = M.keys interm
  -- indicesEncoding is an integer encoding of the keys of interm
  indicesEncoding = M.fromList (zip indices [0..((M.size interm) - 1)])
  -- encodedMap is a conversion of interm where if (n,z) is in the list in interm at value (x,y), 
  -- then (n, encoding of (y,z)) is in the list at encodedMap at value (x,y).
  -- If (y,z) is not a valid key of interm (which may happen at the end of an article), then 
  -- (y,z) gets encoded to -1.
  encodedMap = M.mapWithKey (\k -> map (encode k)) interm where
    encode (x,y) (n,b)
      | M.member (y,b) interm = (n,fromJust (M.lookup (y,b) indicesEncoding))
      | otherwise = (n,-1)

-- |serialize: serializes a ProcessedModel
serialize :: ProcessedModel -> String
serialize = unlines . (map show)