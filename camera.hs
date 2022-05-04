import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSU
import Data.ByteString.Search
import Data.Char

escapeVal str = BS.concat [BS.singleton 0, BSU.pack str, BS.singleton 0]

replaceBinVal original newVal str =
    let escapedVal = escapeVal original
        valLen = BS.length escapedVal
        valPos = head $ indices escapedVal str
        splited = BS.splitAt valPos str
        firstHalf = fst splited
        secondHalf = snd $ BS.splitAt valLen $ snd splited
    in BS.concat [firstHalf, escapeVal newVal, secondHalf]

modConfig original = 
        modCamAngle $ modCamDist1 $ modCamDist2 original
    where
        modCamDist1 = replaceBinVal "1134" "1650"
        modCamDist2 = replaceBinVal "1200" "1650"
        modCamAngle = replaceBinVal "60" "89"

main = do
    original <- withBinaryFile "dota 2 beta\\game\\dota\\bin\\win64\\client.dll" ReadMode BS.hGetContents
    let changed = modConfig original
    withBinaryFile "dota 2 beta\\game\\dota\\bin\\win64\\client.dll" WriteMode $ (\str file -> BS.hPut file str) changed 