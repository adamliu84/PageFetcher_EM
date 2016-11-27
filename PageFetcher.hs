import System.Environment

import Network.HTTP.Conduit
import Network.HTTP
import qualified Data.ByteString.Lazy as LB (unpack)
import Data.Char (chr)

downloadPage ::  String -> IO [String]
downloadPage url = do 
                  return.lines.bsToStr =<< simpleHttp url
                where bsToStr = map (chr . fromEnum).LB.unpack

main :: IO()
main = do
    args <- getArgs
    if (1 <= length args) then 
        do pagehtml <- downloadPage (args!!0)
           print pagehtml
    else
        error $ "Invalid arguments input"
    