import System.Environment
import Control.Monad

import Codec.Picture

import ImageTo2DList
import Asciilate

javaScript :: String
javaScript = "var frames = [].slice.call(document.getElementsByTagName('pre'));var i = 0;setInterval(function() {  frames.forEach(function(frame) {    frame.style.cssText='';  });  document.getElementById(i).style.cssText = 'display:block;';  i = (i+1)%frames.length},150)"

css :: String
css = "pre {display:none; font-family: \"DejaVu Sans Mono\", Monospace; font-size: 8px; line-height: 1em; letter-spacing: calc(1em - 1ex)}"

htmlify :: [String] -> String
htmlify ts =
  "<html><head><style>" ++ css ++ "</style></head><body>" ++
  unlines (map (\(t, index) -> "<pre id=\"" ++ show index ++ "\">" ++ t ++ "</pre>") (zip ts [0..]))
  ++ "<script>" ++ javaScript ++ "</script></body>"

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
    images <- if fileExtension fp == "gif"
          then readGifImages fp
          else fmap (fmap (:[])) $ readImage fp
    case images of
        Right images' -> do
          let asciis = map (unlines . reverse . asciilate . imageTo2DList) images'
          if null destination
            then mapM_ putStrLn asciis
          else if fileExtension destination == "html"
            then do
              writeFile destination $
                htmlify asciis
              putStrLn "Successfully wrote to .hmtl file!"
          else do
            writeFile destination $ unlines asciis
            putStrLn "Successfully wrote to file!"
        _            -> putStrLn "Error reading image"
