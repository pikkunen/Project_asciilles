import System.Environment
import Control.Monad
import Codec.Picture
import ImageTo2DList
import Asciilate


htmlHead :: String
htmlHead = "<html><head><style>body {font-family: \"DejaVu Sans Mono\", Monospace; font-size: 8px; line-height: 1em; letter-spacing: calc(1em - 1ex)}</style></head><body><pre>"

htmlFoot :: String
htmlFoot = "</pre></body></html>"

fileExtension :: String -> String
fileExtension fp =
  let ext = reverse . takeWhile (/='.') . reverse $ fp
  in  if ext == fp
        then ""
        else ext

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
          else if fileExtension destination == "html"
            then do
              writeFile destination $ htmlHead ++ ascii ++ htmlFoot
              putStrLn "Successfully wrote to .hmtl file!"
          else do
            writeFile destination ascii
            putStrLn "Successfully wrote to file!"
        _            -> putStrLn "Error"
