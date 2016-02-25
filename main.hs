import System.Environment
import Codec.Picture
import ImageTo2DList
import Asciilate

main :: IO ()
main = do
    args <- getArgs
    let fp = head args
    image <- readImage fp
    case image of
        Right image' -> putStrLn . unlines . reverse . asciilate . imageTo2DList $ image'
        _            -> putStrLn "Error"
