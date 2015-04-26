import Distribution.Simple
import Control.Monad
import Network.HTTP
import Network.Stream
import Text.HTML.TagSoup
import qualified Data.Map as M
import Data.Char

{-main = simpleHTTP (getRequest url) >>= getResponseBody >>= putStr
where url = "http://wiki.haskell.org/Typeclassopedia"-}

-- TODO: replace 2urls.txt for testing purposes to urls.txt
main = do
  urls <- readFile "2urls.txt" >>= (return.lines)
  wepage_responses <- forM urls (\url -> simpleHTTP (getRequest url))
  raw_data <- mapM getResponseBody wepage_responses -- I should probably change this to catch and warn of connection errors
  let raw_text = map removeHTMLCrap raw_data in
    print $ process raw_text
    
type PrimitiveModel = M.Map (String,String) [String]

type ProcessedModel = [(String,[(Int,Int)])]
    
-- TODO: Make sure that adding filter isAscii at the beginning doesn't break words from webpages too horribly, like turning Gödel into Gdel.
removeHTMLCrap :: String -> String
removeHTMLCrap = extractText . extractDivBody . (dropWhile (/= TagOpen "div" [("id","body")])). canonicalizeTags . parseTags . (filter isAscii) where
--   removeQuotesAndCommas = filter (\x -> notElem x ['\,','"]
  extractText [] = []
  extractText (tag:tags)
    | tag ~== TagText "" = fromTagText tag ++ extractText tags
    | otherwise = extractText tags
  extractDivBody = iterDiv 1 . tail where
    iterDiv 0 _  = []
    iterDiv n ((TagOpen "div" attribs):tags)  = (TagOpen "div" attribs):(iterDiv (n+1) tags)
    iterDiv n ((TagClose "div"):tags) = (TagClose "div"):(iterDiv (n-1) tags)
    iterDiv n (tag:tags) = tag:(iterDiv n tags)
    
process = {-toProcessedModel .-} toPrimitiveModel

-- takes webpage text stripped of its HTML tags and spits out a primitive model.
toPrimitiveModel :: [String] -> PrimitiveModel
toPrimitiveModel s = prim_model where
  stage1 = map words s
  stage2 = map (\x -> (x `zip` tail x) `zip` (map (\a -> [a]) (tail (tail x)))) stage1
  stage3 = concat stage2
  prim_model = M.fromListWith (++) stage3
  
-- Takes a primitive model and spits out a processed model.  
toProcessedModel prim = proc where
  proc = prim
  
  