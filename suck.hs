import Distribution.Simple
import Control.Monad
import Network.HTTP
import Network.Stream
import Text.HTML.TagSoup
import qualified Data.Map as M
import Data.Char
import Data.List
import Data.Maybe

{-main = simpleHTTP (getRequest url) >>= getResponseBody >>= putStr
where url = "http://wiki.haskell.org/Typeclassopedia"-}

-- TODO: replace 2urls.txt for testing purposes to urls.txt
main = do
  urls <- readFile "urls.txt" >>= (return.lines)
  wepage_responses <- forM urls (\url -> simpleHTTP (getRequest url))
  raw_data <- mapM getResponseBody wepage_responses -- I should probably change this to catch and warn of connection errors
  let raw_text = map removeHTMLCrap raw_data in
    print $ serialize $ process raw_text
    
type PrimitiveModel = M.Map (String,String) [String]

type IntermediaryModel = M.Map (String,String) [(Int,String)]

type ProcessedModel = [(String,[(Int,Int)])]
    
-- TODO: Make sure that adding filter isAscii at the beginning doesn't break words from webpages too horribly, like turning Gödel into Gdel.
removeHTMLCrap :: String -> String
removeHTMLCrap = extractText . extractDivBody . (dropWhile (/= TagOpen "div" [("id","body")])). canonicalizeTags . parseTags . (filter isAscii) where
  extractText [] = []
  extractText (tag:tags)
    | tag ~== TagText "" = fromTagText tag ++ extractText tags
    | otherwise = extractText tags
  extractDivBody = iterDiv 1 . tail where
    iterDiv 0 _  = []
    iterDiv n ((TagOpen "div" attribs):tags)  = (TagOpen "div" attribs):(iterDiv (n+1) tags)
    iterDiv n ((TagClose "div"):tags) = (TagClose "div"):(iterDiv (n-1) tags)
    iterDiv n (tag:tags) = tag:(iterDiv n tags)
    
-- big catch-all function for processing data.  Takes nicely formatted text and builds a processed model.
process :: [String] -> ProcessedModel
process = toProcessedModel . toIntermediaryModel . toPrimitiveModel

-- takes webpage text stripped of its HTML tags and spits out a primitive model.
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
  
-- Takes a primitive model and spits out an intermediary model.
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
      
-- Takes an IntermediaryModel and spits out a ProcessedModel.
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

serialize :: ProcessedModel -> String
serialize = (intercalate "\n") . (map show)