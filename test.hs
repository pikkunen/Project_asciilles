import Codec.Picture



main :: IO ()
main = do
    fp <- getLine
    image <- readImage fp
    case image of 
        Right image' -> print . grey . middlePixel $ image'
        _            -> print "Error"

middlePixel (ImageRGB8 image@(Image w h _)) = pixelAt image (w `div` 2) (h `div` 2)

grey (PixelRGB8 r g b) = (r + g + b) `div` 3