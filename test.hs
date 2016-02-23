import Codec.Picture
import Codec.Picture.Types


main :: IO ()
main = do
    fp <- getLine
    image <- readImage fp
    case image of 
        Right image' -> print $ dynamicMap foo image'
        _            -> print "Error"




foo :: Pixel a => Image a -> [[Int]]
foo = 
	snd . pixelFold 
		(\(lastY, ps:pss) x y p -> 
			if y == lastY 
				then (y, (grey p:ps):pss)
				else (y, [grey p]:ps:pss)) 
		(0,[[]])



grey :: Pixel a => a -> Int
grey = go . promotePixel
	where 
		go (PixelRGB8 r g b) = (fromIntegral $ r + b + g) `div` 3