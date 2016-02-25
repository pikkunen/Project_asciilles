import System.Environment
import Control.Monad
import Codec.Picture
import ImageTo2DList
import Asciilate

main :: IO ()
main = do
    (fp:rest) <- getArgs
    let destination = if null rest then "" else head rest
    image <- readImage fp
    case image of
        Right image' -> do
          let ascii = unlines . reverse . asciilate . imageTo2DList $ image'
          putStrLn ascii
          if null destination
            then return ()
            else writeFile destination ascii
        _            -> putStrLn "Error"
