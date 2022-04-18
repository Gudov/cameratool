import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSU
import Data.ByteString.Search
import Data.Char

varName str = BS.concat [BSU.pack str, BS.singleton 0]

replaceBin nameStr val str =
    let name = varName nameStr
        valLen = length val
        nameLen = BS.length name
        namePos = head $ indices name str
        index = nameLen + namePos
        splited = BS.splitAt index str
        nullSplit = BSU.span isControl $ snd splited
    in BS.concat [fst splited, fst nullSplit, BSU.pack val, snd $ BS.splitAt valLen $ snd nullSplit]

modConfig original = 
        modMouseWtf $ modPitchMax $ modVisibleDistance $ modCameraDistanceMin original
    where 
        modCameraDistanceMin = replaceBin "dota_camera_distance_min" "1650"
        modVisibleDistance = replaceBin "r_propsmaxdist" "1650"
        modPitchMax = replaceBin "dota_camera_pitch_max" "89"
        modMouseWtf = replaceBin "dota_camera_mousewheel_direction_multiplier" "89"

main = do
    original <- withBinaryFile "dota 2 beta\\game\\dota\\bin\\win64\\client.dll" ReadMode BS.hGetContents
    let changed = modConfig original
    withBinaryFile "dota 2 beta\\game\\dota\\bin\\win64\\client.dll" WriteMode $ (\str file -> BS.hPut file str) changed 