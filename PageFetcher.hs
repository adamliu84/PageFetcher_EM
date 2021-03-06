import System.Environment (getArgs)
import Network.HTTP.Conduit (simpleHttp)
import Data.Char (chr)
import Control.Monad (when)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Data.ByteString.Lazy as LB (unpack)
import qualified Data.Time as TI

downloadPage ::  String -> IO [String]
downloadPage url = do
                  return.lines.bsToStr =<< simpleHttp url
                where bsToStr = map (chr . fromEnum).LB.unpack

timelogfilename :: String
timelogfilename = "time.log"

writetimelog :: String -> IO()
writetimelog hiturl = do
            curdatetime <- formCurrentTime
            appendFile timelogfilename $ concat[curdatetime," ", hiturl, "\n"]

formCurrentTime:: IO String
formCurrentTime = do
    now <- TI.getCurrentTime
    timezone <- TI.getCurrentTimeZone
    let szCurDate = yymmddCsv.TI.toGregorian.TI.utctDay $ now
        szCurTime = hhmmss.TI.localTimeOfDay $ TI.utcToLocalTime timezone now
    return (szCurDate++"_"++szCurTime)
    where yymmddCsv (y,m,d) =  concat [show y, pad2zero m, pad2zero d]
          hhmmss (TI.TimeOfDay h m s) = concat[pad2zero h, pad2zero m, take 2 $ pad2zero s]
          pad2zero x = if x < 10 then
                        reverse.show $ x * 10
                       else
                        show x

htmllogfilename :: String
htmllogfilename = "html.log"

writehtmllog :: String -> IO()
writehtmllog html = writeFile htmllogfilename html

helptext :: String
helptext = "Usage: PageFetcher target_page [-l] [-p] \
            \ \n\n \
            \Options:\n \
            \\t-l\t\tOutput timestamp with target page\n\
            \\t-p\t\tOutput grabbed html text\n\
            \\t-h\t\tDisplay this help\
            \\n\n\
            \Sample:\n \
            \\tPageFetcher.exe http://www.nosuchpage.com -h -l -p\
            \\n\n"

main :: IO()
main = do
    setLocaleEncoding utf8
    args <- getArgs
    if (1 <= length args) then
        do pagehtml <- downloadPage (args!!0)
            -- Check if required to print help
           when ("-h" `elem` args) $ putStr $ helptext
           -- Check if required to log timestamp
           when ("-l" `elem` args) $ writetimelog (args!!0)
           -- Check if requireed to log the output html
           when ("-p" `elem` args) $ writehtmllog $ concat pagehtml
    else
        do putStr $ helptext ++ "\n"
           error $ "Invalid arguments input"
