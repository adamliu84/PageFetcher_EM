import System.Environment
import Network.HTTP.Conduit
import Network.HTTP
import qualified Data.ByteString.Lazy as LB (unpack)
import Data.Char (chr)
import Data.Time
import Data.Time.Format
import System.IO.Unsafe
import Data.List.Split
import Data.String.Utils (replace)
import Data.Time.Clock
import Data.Time.LocalTime

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
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let szCurDate = yymmddCsv.toGregorian.utctDay $ now
        szCurTime = hhmmss.localTimeOfDay $ utcToLocalTime timezone now
    return (szCurDate++"_"++szCurTime)
    where yymmddCsv (y,m,d) =  concat [show y, pad2zero m, pad2zero d]
          hhmmss (TimeOfDay h m s) = concat[pad2zero h, pad2zero m, take 2 $ pad2zero s]
          pad2zero x = if x < 10 then
                        reverse.show $ x * 10
                       else
                        show x     
                
htmllogfilenme :: String                
htmllogfilenme = "html.log"

writehtmllog :: String -> IO()
writehtmllog html = writeFile htmllogfilenme html
             
main :: IO()
main = do
    args <- getArgs
    if (1 <= length args) then 
        do pagehtml <- downloadPage (args!!0)
        
           -- Check if required to log timestamp
           if ("-l" `elem` args) then
            writetimelog (args!!0)
           else
            return ()
            
           -- Check if requireed to log the output html
           if ("-p" `elem` args) then
            writehtmllog $ concat pagehtml
           else
            return ()
    else
        error $ "Invalid arguments input"
    