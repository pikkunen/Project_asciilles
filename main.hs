import System.Environment

import Control.Monad
import Data.Maybe

import Codec.Picture
import Codec.Picture.Metadata

import ImageTo2DList
import Asciilate

-- if the output is html, add some JavaScript, CSS and wrap everthing
-- in a pre-element so that it looks pretty and animates in the case of a gif
htmlify :: [String] -> String
htmlify ts =
  "<html><head><style>" ++ css ++ "</style></head><body>" ++
  unlines (map (\(t, index) -> "<pre id=\"" ++ show index ++ "\">" ++ t ++ "</pre>") (zip ts [0..]))
  ++ "<script>" ++ javaScript ++ "</script></body>"
  where
    javaScript = "var frames = [].slice.call(document.getElementsByTagName('pre'));var i = 0;setInterval(function() {frames.forEach(function(frame) {frame.style.cssText='';});document.getElementById(i).style.cssText = 'display:block;';i = (i+1)%frames.length},150)"
    css = "pre {display:none; font-family: \"DejaVu Sans Mono\", Monospace; font-size: 8px; line-height: 1em; letter-spacing: calc(1em - 1ex)}"


-- in the case of a file path, get the extension (returns an empty string in the case of no dots)
fileExtension :: String -> String
fileExtension fp =
  let ext = reverse . takeWhile (/='.') . reverse $ fp
  in  if ext == fp
        then ""
        else ext

main :: IO ()
main = do

  (fp:args) <- getArgs

  let (destination, scale) = findArguments args
  let scale' = fromMaybe 3 scale

  images <- if fileExtension fp == "gif"
        then readGifImages fp
        else fmap (fmap (:[])) $ readImage fp

  either
    putStrLn
    (\images' -> do
      let asciis = map (unlines . asciilate scale' . imageTo2DList) images'
      maybe
        (mapM_ putStrLn asciis)
        (\destination' ->
          if fileExtension destination' == "html"
            then do
              writeFile destination' $ htmlify asciis
              putStrLn "Successfully wrote to .hmtl file!"
            else do
              writeFile destination' $ unlines asciis
              putStrLn "Successfully wrote to file!")
         destination)
      images

  where
    -- find arguments looks through the command line arguments and finds the destination and scale ()
    findArguments :: [String] -> (Maybe String, Maybe Int)
    findArguments = foldl
      (\(oldDest, oldScale) arg ->
        if all (`elem` ['0'..'9']) arg
          then (oldDest , Just $ read arg)
          else (Just arg, oldScale       ))
      (Nothing, Nothing)
