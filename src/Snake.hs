module Snake (
  Cell(..),
  Snake(..),
  Direction(..),
  updateSnake,
  changeSnakeDirection,
  snakeHeadSelfCollision,
  reverseDirection,
  increaseSnakeLength,
  incrementSnakeLength
)where

import Graphics.UI.SDL.Renderable
import Graphics.UI.SDL as SDL
import SnakeMap

data Direction = DirLeft | DirRight | DirUp | DirDown deriving (Eq)


data Snake = Snake {
                    snakeDirection :: Direction,
                    snakeHead :: Cell,
                    snakeTail :: [Cell]
                    } deriving (Eq)

headColor :: Color
headColor = Color 255 0 0

tailColor :: Color
tailColor = Color 0 255 0

instance Renderable Snake where
  render (Snake _ sHead sTail) ontoSurface = do
                                            render headCCell ontoSurface
                                            renderTail sTail ontoSurface
                                            where headCCell = ColoredCell sHead headColor
                                                  renderTail [] _ = return ()
                                                  renderTail (tailStart:tailEnd) surface = do
                                                                                            render (ColoredCell tailStart tailColor) surface
                                                                                            renderTail tailEnd surface
                                            

                            


                                                            
changeSnakeDirection :: Snake -> Direction -> Snake
changeSnakeDirection s@(Snake _ h t) dir | moveCell dir h `elem` t = s
                                         | otherwise = Snake dir h t 

-- | Moves the snake forward in the direction it is travelling
updateSnake :: Snake -> Snake
updateSnake (Snake direction sHead sTail) = Snake direction (moveCell direction sHead) $ moveCellList sHead sTail

-- | Moves a single cell in the given direction
moveCell :: Direction -> Cell -> Cell
moveCell DirLeft (Cell x y) = Cell (x - 1) y
moveCell DirRight (Cell x y) = Cell (x + 1) y
moveCell DirUp (Cell x y) = Cell x $ y - 1
moveCell DirDown (Cell x y) = Cell x $ y + 1

moveCellList :: Cell -> [Cell] -> [Cell]
moveCellList nextStart list = moveCellListRecurse nextStart list []
                           where moveCellListRecurse _ [] moved = moved
                                 moveCellListRecurse firstTo (first:rest) moved = moveCellListRecurse first rest $ moved ++ [firstTo]

reverseDirection :: Direction -> Direction
reverseDirection DirLeft = DirRight
reverseDirection DirRight = DirLeft
reverseDirection DirUp = DirDown
reverseDirection DirDown = DirUp

snakeHeadSelfCollision:: Snake -> Bool
snakeHeadSelfCollision (Snake _ sHead sTail) = sHead `elem` sTail

increaseSnakeLength :: Int -> Snake -> Snake
increaseSnakeLength 0 s = s
increaseSnakeLength amt (Snake dir h []) = increaseSnakeLength (amt - 1) $ Snake dir h [moveCell (reverseDirection dir) h]
increaseSnakeLength amt (Snake dir h t) = increaseSnakeLength (amt - 1) $ Snake dir h $ t ++ [last t]

incrementSnakeLength :: (Snake -> Snake)
incrementSnakeLength = increaseSnakeLength 1
