module ImageTo2DList (imageTo2DList) where

-- Codec.Picture comes from the JuicyPixels library (https://hackage.haskell.org/package/JuicyPixels-3.2.7)
import Codec.Picture
import Codec.Picture.Types

import Utils

import qualified GHC.Word as W

import Test.QuickCheck hiding (NonEmptyList)
import Test.HUnit

{- functionIdentifier y a size
   PURPOSE:  If a pixel has a transperency component, simply throwing it away can be missleading,
             especially in the case of a completely transparent pixel, which are often "black", but excpected to
             be displayed on a white background. This function takes a grey value, a transperency and a size
             and mixes it with white, where the amound of white is greater if the pixel is more transparent.
             The size is the value of the highest grey and alpha values (so 255 for PixelYA8 etc.)
   PRE:      size is not 0
   POST:     a mix of white and y where a is the "weight" that is placed upon y in the mixing process
   EXAMPLES: fillTransparency 0 0 255 = 255
             fillTransparency 0 255 255 = 0
             fillTransparency 0 127 255 = 128
-}
fillTransparency :: (Integral a, Integral b, Integral c, Integral d) => a -> b -> c -> d
fillTransparency y a size =
  let y' = fromIntegral y;
      a' = fromIntegral a;
      size' = fromIntegral size
  in  round $ (y' * a' + size' * (size'-a')) / size'

-- converts a DynamicImage to another DynamicImage with the same resolution, but in greyscale and without transperency
-- (fillTransparency is used on images with a pixel component) does not support CMYK8 or CMYK16, but all other
-- juicypixel color formats
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

-- used in conjunction with toGrey to convert images of different format to Image8 format
-- img must be convertible by toGrey
toImage8 :: DynamicImage -> Image Pixel8
toImage8 img = case toGrey img of
  ImageY8  img' -> img'
  ImageY16 img' -> pixelMap (\n -> fromIntegral $ 1 + ((n-1) `div` 256)) img'
  ImageYF  img' -> pixelMap (round . (*255)) img'

-- like imageTo2DList, but works only on ImageY8
pixelMatrix :: Image Pixel8 -> [[Int]]
pixelMatrix =
	map reverse . reverse .  snd . pixelFold -- because of the direction pixelFold works in, and the direction
		(\(lastY, ps:pss) x y p ->             -- you add things to lists, reverse and map reverse are necessary
			if y == lastY                        -- to make the output not mirrored horizontaly and vertically
				then (y, (fromIntegral p:ps):pss)
				else (y, [fromIntegral p]:ps:pss))
		(0,[[]])


{- imageTo2DList image
   PURPOSE:  Transform an image into a 2DList of values.
   PRE:      image is convertible by toImage8
   POST:     a 2DList of values 0-255 where every value correspond to a pixel in the image
-}
imageTo2DList = pixelMatrix . toImage8


-- the following is used only for testing

-- the inverse of imageTo2DList
list2DtoImage lss =
   let w = length $ head lss
       h = length lss
   in  ImageY8 $ generateImage (\x y -> lss !! y !! x) w h

quickChecks =
  sequence_
    [quickCheck ((\y a -> let y' = fillTransparency y a 255 in y' <= 255 && y' >= 0) :: W.Word8 -> W.Word8 -> Bool)
      -- the resulting grey value should be between 0 and 255
    ,quickCheck (\lss ->
       map (map fromIntegral) (getList2D lss) == (imageTo2DList . list2DtoImage. getList2D) lss)
      -- turning a 2dlist into an image and back again should result in the original list
    ]

hUnitTests =
  runTestTT . TestList $
    [TestCase $ assertEqual ("mixing black with white in (almost equal) parts") 128 (fillTransparency 0 127 255)
    ,TestCase $ assertEqual ("completely transparent -> white") 255 (fillTransparency 0 0 255)
    ]

runtests = do
  putStrLn "HUnit:"
  hUnitTests
  putStrLn "QuickCheck:"
  quickChecks
