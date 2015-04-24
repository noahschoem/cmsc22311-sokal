import Distribution.Simple
import Control.Monad
import Network.HTTP
import Network.Stream
import Text.HTML.TagSoup
import SuckUtils

{-main = simpleHTTP (getRequest url) >>= getResponseBody >>= putStr
where url = "http://wiki.haskell.org/Typeclassopedia"-}

main = do
  urls <- readFile "1url.txt" >>= (return.lines)
  wepage_responses <- forM urls (\url -> simpleHTTP (getRequest url))
  raw_data <- mapM getResponseBody wepage_responses-- I should probably change this to catch and warn of connection errors
  print $ head $ map process raw_data
    --let a = process raw_data in
    --    writeFile "sokal.data" $ show a
    --    return ()
  
process = findOuterCloseDiv . (dropWhile (/= TagOpen "div" [("id","body")])). canonicalizeTags . parseTags where
  findOuterCloseDiv = iterDiv 1 . tail where
    iterDiv 0 _  = []
    iterDiv (-1) _ = error "iterDiv failure"
    iterDiv n ((TagOpen "div" attribs):tags)  = (TagOpen "div" attribs):(iterDiv (n+1) tags)
    iterDiv n ((TagClose "div"):tags) = (TagClose "div"):(iterDiv (n-1) tags)
    iterDiv n (tag:tags) = tag:(iterDiv n tags)
  
-- type PrimitiveModel = Map (String,String) [String]

-- type ProcessedModel = [(String,[(Int,Int)])]
  
{-  
-- responsible only for stripping tags from a Response.
stripTags :: Response -> String
process  = extractBody . removeAttributeDivs . canonicalizeTags . parseTags . rspBody where
  removeAttributeDivs tagSoup = 
  extractBody = (takeWhile != TagClose "div" []) . tail . (dropWhile (!= TagOpen "div" ("id","body")))
  
-- takes the webpage text stripped of its tags and spits out a primitive model.
toPrimitiveModel :: [String] -> PrimitiveModel
toPrimitiveModel = toPrimitiveModel

-- Takes a primitive model and spits out a processed model.
toProcessedModel :: PrimitiveModel -> ProcessedModel
toProcessedModel = toProcessedModel-}