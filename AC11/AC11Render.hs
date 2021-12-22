module AC11Render where

import Graphics.Gloss
--import AC11_1
import qualified Data.Map as M

screenDim :: Int
screenDim = 500

size :: Int
size = 10

scaleBy :: Float
scaleBy = (fromIntegral screenDim :: Float) / (fromIntegral size :: Float)

modelToPicture :: M.Map (Int, Int) Int -> Picture
modelToPicture = pictures . map coordToRect . M.toList

grayScaleVal :: Int -> Color
grayScaleVal x = makeColor 1.0 1.0 1.0 ((fromIntegral x) / 10)

coordToRect :: ((Int, Int), Int) -> Picture
coordToRect ((x, y), g) = 
      color (grayScaleVal g) . 
      translate (translateBy x) (translateBy y) .
      scale scaleBy scaleBy . 
      polygon $
      rectanglePath 1 1
         where translateBy a = (-1)*( (fromIntegral screenDim :: Float) / 2) + scaleBy*(fromIntegral a :: Float) - scaleBy / 2