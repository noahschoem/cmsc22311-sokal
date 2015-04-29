import Distribution.Simple
import Control.Monad
import Network.HTTP
import Network.Stream
import Text.HTML.TagSoup
import Data.Char (isAscii)
import SuckUtils as SuckUtils

main = do
  urls <- readFile "urls.txt" >>= (return.lines)
  wepage_responses <- forM urls (\url -> simpleHTTP (getRequest url))
  -- Note the unsafe use of getResponseBody.  If a given webpage response is a ConnError, then this program will crash.
  raw_data <- mapM getResponseBody wepage_responses
  let raw_text = map extractPaperText raw_data in
    writeFile "sokal.model" $ SuckUtils.processAndSerialize raw_text
   
-- |extractPaperText: responsible for extracting the text we care about a webpage on Project MUSE.
-- Note that this extracts only the body of an article and ignores abstracts and footers.
extractPaperText :: String -> String
extractPaperText = extractText . extractDivBody . (dropWhile (/= TagOpen "div" [("id","body")])). canonicalizeTags . parseTags . (filter isAscii) where
  extractText [] = []
  extractText (tag:tags)
    | tag ~== TagText "" = fromTagText tag ++ extractText tags
    | otherwise = extractText tags
  extractDivBody = iterDiv 1 . tail where
    iterDiv 0 _  = []
    iterDiv n ((TagOpen "div" attribs):tags)  = (TagOpen "div" attribs):(iterDiv (n+1) tags)
    iterDiv n ((TagClose "div"):tags) = (TagClose "div"):(iterDiv (n-1) tags)
    iterDiv n (tag:tags) = tag:(iterDiv n tags)
    
