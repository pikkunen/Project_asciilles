import Codec.Picture

main :: IO ()
main = do
    fp <- getLine
    image <- readImage fp
    case image of
        Right image' -> print . pixelMatrix . toImage8 $ image'
        _            -> print "Error"

toImage8 :: DynamicImage -> Image Pixel8
toImage8 (ImageRGB8  img) = pixelMap (\(PixelRGB8  r g b) -> (fromIntegral $ r + g + b) `div` 3      ) img
toImage8 (ImageRGB16 img) = pixelMap (\(PixelRGB16 r g b) -> (fromIntegral $ r + g + b) `div` (3*256)) img
-- etc.


pixelMatrix ::  Image Pixel8 -> [[Int]]
pixelMatrix =
	snd . pixelFold
		(\(lastY, ps:pss) x y p ->
			if y == lastY
				then (y, (fromIntegral p:ps):pss)
				else (y, [fromIntegral p]:ps:pss))
		(0,[[]])
