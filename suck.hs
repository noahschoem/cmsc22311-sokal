import Distribution.Simple
import Control.Monad
import Network.HTTP
import Network.Stream
import Text.HTML.TagSoup
import SuckUtils as SuckUtils

{-main = simpleHTTP (getRequest url) >>= getResponseBody >>= putStr
where url = "http://wiki.haskell.org/Typeclassopedia"-}

-- TODO: replace 2urls.txt for testing purposes to urls.txt
main = do
  urls <- readFile "2urls.txt" >>= (return.lines)
  wepage_responses <- forM urls (\url -> simpleHTTP (getRequest url))
  -- Note the unsafe use of getResponseBody.  If a given webpage response is a ConnError, then this program will fail.
  raw_data <- mapM getResponseBody wepage_responses
  let raw_text = map removeHTMLCrap raw_data in
    writeFile "sokal.model" $ SuckUtils.serializeAndProcess raw_text
   
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
    
