module ImageTo2DList (imageTo2DList) where

import Codec.Picture
import Codec.Picture.Types

average :: Integral a => [a] -> a
average ls = foldl (+) 0 ls `div` fromIntegral (length ls)

-- takes a grey value, a alph
fillTransparency y a size =
  let y' = fromIntegral y;
      a' = fromIntegral a;
      size' = fromIntegral size
  in  round $ (y' * a' + size' * (size'-a')) / size'

toGrey :: DynamicImage -> DynamicImage
toGrey img = case img of
  ImageY8  img' -> ImageY8  img'
  ImageY16 img' -> ImageY16 img'
  ImageYF  img' -> ImageYF  img'

  ImageYA8  img' -> ImageY8  $ pixelMap (\(PixelYA8  y a) -> fillTransparency y a 256  ) img'
  ImageYA16 img' -> ImageY16 $ pixelMap (\(PixelYA16 y a) -> fillTransparency y a 65536) img'

  ImageRGB8  img' -> ImageY8  $ pixelMap (\(PixelRGB8  r g b) -> average [r,g,b]) img'
  ImageRGB16 img' -> ImageY16 $ pixelMap (\(PixelRGB16 r g b) -> average [r,g,b]) img'
  ImageRGBF  img' -> ImageYF  $ pixelMap (\(PixelRGBF  r g b) -> (r+b+g)/3) img'

  ImageRGBA8  img' -> ImageY8  $ pixelMap (\(PixelRGBA8  r g b a) -> fillTransparency (average [r,g,b]) a 255  ) img'
  ImageRGBA16 img' -> ImageY16 $ pixelMap (\(PixelRGBA16 r g b a) -> fillTransparency (average [r,g,b]) a 65535) img'

  ImageYCbCr8 img' -> ImageY8 $ pixelMap (\(PixelYCbCr8 y _ _) -> y) img'

  _ -> error "unsupported color format"

toImage8 :: DynamicImage -> Image Pixel8
toImage8 img = case toGrey img of
  ImageY8  img' -> img'
  ImageY16 img' -> pixelMap (\n -> fromIntegral $ 1 + ((n-1) `div` 256)) img'
  ImageYF  img' -> pixelMap (round . (*255)) img'


pixelMatrix ::  Image Pixel8 -> [[Int]]
pixelMatrix =
	snd . pixelFold
		(\(lastY, ps:pss) x y p ->
			if y == lastY
				then (y, (fromIntegral p:ps):pss)
				else (y, [fromIntegral p]:ps:pss))
		(0,[[]])

imageTo2DList = pixelMatrix . toImage8
