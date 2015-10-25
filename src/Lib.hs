module Lib
    ( someFunc
    ) where

import Data.List.Split (splitOn)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (catMaybes)
import Control.Monad (mapM_)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception.Base (try, catch)
import Network.HTTP.Conduit (simpleHttp, HttpException)
import qualified Data.ByteString.Lazy as LB
import Codec.Picture (decodeImage, DynamicImage(ImageYCbCr8), imageData, pixelAt)
import Codec.Picture.Types (Image(Image), Pixel, PixelYCbCr8(PixelYCbCr8))

type Url = String
type Hue = Float


getUrls :: String -> [Url]
getUrls input =
  let lines = splitOn "data-src=\"" input
  in fmap (head . splitOn "\"") $ filter (isPrefixOf "https://") lines

process :: Url -> IO (Maybe (Url, Hue))
process url = do
  putStrLn ("Fetching " ++ url)
  result <- LB.toStrict <$> simpleHttp url
  return $ case decodeImage result of
    Left _ -> Nothing
    Right image -> fmap ((,) url) $ getImageHue image

safeProcess :: Url -> IO (Maybe (Url, Hue))
safeProcess url = do
  result <- try (process url) :: IO (Either HttpException (Maybe (Url, Hue)))
  case result of
    Left _ -> return Nothing
    Right x -> return x

getImageHue :: DynamicImage -> Maybe Hue
getImageHue (ImageYCbCr8 image@(Image w h _)) =
  let pixels = [ pixelAt image x y | x <- [0..w-1], y <- [0..h-1] ]
      hue = (sum $ fmap f pixels) / fromIntegral (w * h)
      lum = (sum $ fmap g pixels) / fromIntegral (w * h)
  in if lum > 0.3 && lum < 0.8
     then Just $ (sum $ fmap f pixels) / fromIntegral (w * h)
     else Nothing
  where f (PixelYCbCr8 y cb cr) = atan2 (fromIntegral cb / 255) (fromIntegral cr / 255)
        g (PixelYCbCr8 y cb cr) = fromIntegral y / 255
getImageHue _ = Nothing

rgbToHue :: Float -> Float -> Float -> Hue
rgbToHue r g b
  | r == max = (g - b) / (max - min)
  | g == max = 2 + (b - r) / (max - min)
  | b == max = 4 + (r - g) / (max - min)
  where max = maximum [r, g, b]
        min = minimum [r, g, b]

someFunc :: IO ()
someFunc = do
  file <- readFile "input2.html"
  let urls = take 500 $ getUrls file
  images <- catMaybes <$> mapConcurrently process urls
  let sortedImages = sortOn snd images
  let output = unlines $ fmap (\(url, _) -> "<img src=\"" ++ url ++ "\">") sortedImages
  let style = "<style>\n"
           ++ "  img {float: left; width: 150px; height: 150px;}\n"
           ++ "  img:nth-child(6n+1) {clear: both;}\n"
           ++ "</style>\n"
  writeFile "output.html" $ style ++ output
  return ()
