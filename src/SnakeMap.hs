module SnakeMap (
  SnakeMap(..),
  Cell(..),
  ColoredCell(..),
  cellSide
)where

import Graphics.UI.SDL.Renderable
import qualified Graphics.UI.SDL.RenderUtils as RU
import qualified Graphics.UI.SDL.Primitives as SDLGFX
import Graphics.UI.SDL as SDL
import Data.Int

data Cell = Cell Int Int deriving (Eq)

data SnakeMap = SnakeMap {
                  width :: Int,
                  height :: Int,
                  food :: Cell
                  }

-- | The side length of a single cell in pixels
cellSide :: Int
cellSide = 8


asSurfaceCoord :: Int -> Int16
asSurfaceCoord x = fromIntegral $ x*cellSide

partRadius :: Int16
partRadius = fromIntegral $ cellSide `div` 2

foodColor :: Color
foodColor = Color 0 0 255
data ColoredCell = ColoredCell Cell Color

instance Renderable ColoredCell where
  render (ColoredCell (Cell x y) color) ontoSurface = do
                                                       let pixelCol =  RU.colorAsGFXWord color
                                                       _ <- SDLGFX.filledCircle ontoSurface surfaceX surfaceY partRadius pixelCol
                                                       return ()
                                                      where surfaceX = fromIntegral $ partRadius + asSurfaceCoord x
                                                            surfaceY = fromIntegral $ partRadius + asSurfaceCoord y 
instance Renderable SnakeMap where
  render (SnakeMap _ _ foodCell) onto = render (ColoredCell foodCell foodColor) onto